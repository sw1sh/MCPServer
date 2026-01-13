(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`MCPServer`DefaultServers`" ];
Begin[ "`Private`" ];

Needs[ "Wolfram`MCPServer`"        ];
Needs[ "Wolfram`MCPServer`Common`" ];

Needs[ "Wolfram`MCPServer`CreateMCPServer`" -> None ];
Needs[ "Wolfram`Chatbook`" -> "cb`" ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Configuration*)
$defaultMCPServer = "Wolfram";

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Default Tools*)
$DefaultMCPTools := WithCleanup[
    Unprotect @ $DefaultMCPTools,
    $DefaultMCPTools = AssociationMap[ Apply @ Rule, $defaultMCPTools ],
    Protect @ $DefaultMCPTools
];

$defaultMCPTools = <| |>;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*ReadNotebook*)
$defaultMCPTools[ "ReadNotebook" ] := LLMTool @ <|
    "Name"        -> "ReadNotebook",
    "DisplayName" -> "Read Notebook",
    "Description" -> "Reads the contents of a Wolfram notebook (.nb) as markdown text.",
    "Function"    -> readNotebook,
    "Options"     -> { },
    "Parameters"  -> {
        "notebook" -> <|
            "Interpreter" -> "String",
            "Help"        -> "The Wolfram notebook to read, specified as a file path or a NotebookObject[...]",
            "Required"    -> True
        |>
    }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*readNotebook*)
readNotebook // beginDefinition;

readNotebook[ KeyValuePattern[ "notebook" -> notebook_ ] ] :=
    readNotebook @ notebook;

readNotebook[ file_String ] /; FileExistsQ @ file := Enclose[
    Catch @ Module[ { nb },
        nb = Import[ file, "NB" ];
        If[ ! MatchQ[ nb, _Notebook ], Throw[ "File is not a valid Wolfram notebook: " <> file ] ];
        ConfirmMatch[ chatbookVersionCheck[ ], True, "ChatbookVersionCheck" ];
        ConfirmBy[ exportMarkdownString @ nb, StringQ, "Result" ]
    ],
    throwInternalFailure
];

readNotebook[ nbo0_String ] := Enclose[
    Catch @ Module[ { held, nbo },
        held = Quiet @ ToExpression[ nbo0, InputForm, HoldComplete ];
        If[ ! MatchQ[ held, HoldComplete[ NotebookObject[ __String ] ] ],
            Throw[ "Invalid notebook specification: " <> nbo0 ]
        ];
        nbo = ConfirmMatch[ ReleaseHold @ held, NotebookObject[ __String ], "NotebookObject" ];
        ConfirmMatch[ chatbookVersionCheck[ ], True, "ChatbookVersionCheck" ];
        ConfirmBy[ exportMarkdownString @ nbo, StringQ, "Result" ]
    ],
    throwInternalFailure
];

readNotebook // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*exportMarkdownString*)
importResourceFunction[ exportMarkdownString, "ExportMarkdownString" ];

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WriteNotebook*)
$defaultMCPTools[ "WriteNotebook" ] := LLMTool @ <|
    "Name"        -> "WriteNotebook",
    "DisplayName" -> "Write Notebook",
    "Description" -> "Converts markdown text to a Wolfram notebook and saves it to a file.",
    "Function"    -> writeNotebook,
    "Options"     -> { },
    "Parameters"  -> {
        "file" -> <|
            "Interpreter" -> "String",
            "Help"        -> "The file to write the notebook to (must end in .nb).",
            "Required"    -> True
        |>,
        "overwrite" -> <|
            "Interpreter" -> "Boolean",
            "Help"        -> "Whether to overwrite an existing file (default is False).",
            "Required"    -> False
        |>,
        "markdown" -> <|
            "Interpreter" -> "String",
            "Help"        -> "The markdown text to write to a notebook.",
            "Required"    -> True
        |>
    }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*writeNotebook*)
writeNotebook // beginDefinition;

writeNotebook[ KeyValuePattern @ { "markdown" -> markdown_, "file" -> file_, "overwrite" -> overwrite_ } ] :=
    writeNotebook[ markdown, file, TrueQ @ overwrite ];

writeNotebook[ markdown_String, file_String, overwrite: True|False ] := Enclose[
    Catch @ Module[ { nb },
        If[ FileExistsQ @ file && ! overwrite, Throw[ "File already exists: " <> file ] ];
        ConfirmMatch[ chatbookVersionCheck[ ], True, "ChatbookVersionCheck" ];
        nb = ConfirmMatch[ importMarkdownString[ markdown, "Notebook" ], _Notebook, "Notebook" ];
        ConfirmBy[ Export[ file, nb, "NB" ], FileExistsQ, "File" ]
    ],
    throwInternalFailure
];

writeNotebook // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*importMarkdownString*)
importResourceFunction[ importMarkdownString, "ImportMarkdownString" ];

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WolframAlpha*)
$defaultMCPTools[ "WolframAlpha" ] := LLMTool @ <|
    "Name"        -> "WolframAlpha",
    "DisplayName" -> "Wolfram|Alpha",
    "Description" -> $wolframAlphaToolDescription,
    "Function"    -> Function[ cb`$DefaultTools[ "WolframAlpha" ][ # ][ "String" ] ],
    "Options"     -> { },
    "Parameters"  -> {
        "query" -> <|
            "Interpreter" -> "String",
            "Help"        -> "the input",
            "Required"    -> True
        |>
    }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WolframLanguageEvaluator*)
$defaultMCPTools[ "WolframLanguageEvaluator" ] := LLMTool @ <|
    "Name"        -> "WolframLanguageEvaluator",
    "DisplayName" -> "Wolfram Language Evaluator",
    "Description" -> $wolframLanguageEvaluatorToolDescription,
    "Function"    -> evaluateWolframLanguage,
    "Options"     -> { },
    "Parameters"  -> {
        "code" -> <|
            "Interpreter" -> "String",
            "Help"        -> "The Wolfram Language code to evaluate.",
            "Required"    -> True
        |>,
        "timeConstraint" -> <|
            "Interpreter" -> "Integer",
            "Help"        -> "The time constraint for the evaluation (default is 60 seconds).",
            "Required"    -> False
        |>
    }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*evaluateWolframLanguage*)
evaluateWolframLanguage // beginDefinition;

evaluateWolframLanguage[ KeyValuePattern @ { "code" -> code_, "timeConstraint" -> timeConstraint_ } ] :=
    evaluateWolframLanguage[ code, timeConstraint ];

evaluateWolframLanguage[ code_String, _Missing ] :=
    evaluateWolframLanguage[ code, 60 ];

evaluateWolframLanguage[ code_String, timeConstraint_Integer ] := Enclose[
    Module[ { string, exported },
        ConfirmMatch[ chatbookVersionCheck[ ], True, "ChatbookVersionCheck" ];
        string   = ConfirmBy[ evaluateWolframLanguage0[ code, timeConstraint ], StringQ, "Result" ];
        exported = ConfirmBy[ exportImages @ string, StringQ, "Result" ];
        StringTrim @ exported
    ],
    throwInternalFailure
];

evaluateWolframLanguage // endDefinition;


evaluateWolframLanguage0 // beginDefinition;

(* :!CodeAnalysis::BeginBlock:: *)
(* :!CodeAnalysis::Disable::PrivateContextSymbol:: *)
(* :!CodeAnalysis::Disable::SuspiciousSessionSymbol:: *)
evaluateWolframLanguage0[ code_String, timeConstraint_Integer ] :=
    Block[
        {
            Wolfram`Chatbook`Sandbox`Private`$evaluatorMethod          = "Session",
            Wolfram`Chatbook`Sandbox`Private`appendURIInstructions     = # &,
            Wolfram`Chatbook`Sandbox`Private`appendRetryNotice         = # &,
            Wolfram`Chatbook`Common`$toolResultStringLength            = 10000,
            Wolfram`Chatbook`Sandbox`Private`$sandboxEvaluationTimeout = timeConstraint,
            $Line = $line++
        },
        Wolfram`Chatbook`Common`catchTop @ Wolfram`Chatbook`Common`sandboxEvaluate[ StackBegin @ code ][ "String" ]
    ];
(* :!CodeAnalysis::EndBlock:: *)

evaluateWolframLanguage0 // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*exportImages*)
exportImages // beginDefinition;

exportImages[ str_String ] := Enclose[
    Module[ { content, hasImages, exported, result },

        content = ConfirmMatch[ cb`GetExpressionURIs @ str, { __ }, "Content" ];

        hasImages = False;
        exported = ConfirmMatch[
            Replace[ content, expr: Except[ _String ] :> (hasImages = True; exportImage @ expr), { 1 } ],
            { __String },
            "Exported"
        ];

        result = ConfirmBy[ StringJoin @ exported, StringQ, "Result" ];

        If[ TrueQ @ hasImages,
            result <> "\n\n" <> $markdownImageHint,
            result
        ]
    ],
    throwInternalFailure
];

exportImages // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*exportImage*)
exportImage // beginDefinition;

exportImage[ expr_ ] /; $imageExportMethod === "Local" := Enclose[
    Module[ { hash, file, png, lo, uri },
        hash = ConfirmBy[ Hash[ expr, Automatic, "HexString" ], StringQ, "Hash" ];
        file = ConfirmBy[ fileNameJoin[ $imagePath, StringTake[ hash, 3 ], hash <> ".png" ], fileQ, "File" ];
        png  = ConfirmBy[ Export[ file, expr, "PNG" ], FileExistsQ, "PNG" ];
        lo   = ConfirmMatch[ LocalObject @ png, HoldPattern @ LocalObject[ _String, ___ ], "LocalObject" ];
        uri  = ConfirmBy[ First @ lo, StringQ, "URI" ];
        "![Image]("<>uri<>")"
    ],
    throwInternalFailure
];

exportImage[ expr_ ] := Enclose[
    Module[ { hash, root, file, png, uri },

        hash = ConfirmBy[ Hash[ expr, Automatic, "HexString" ], StringQ, "Hash" ];
        root = ConfirmMatch[ $cloudImagePath, CloudObject[ _String, ___ ], "Root" ];

        file = ConfirmMatch[
            FileNameJoin @ { root, StringTake[ hash, 3 ], hash <> ".png" },
            _CloudObject,
            "File"
        ];

        png = ConfirmBy[ Export[ file, expr, "PNG" ], FileExistsQ, "PNG" ];

        uri = First @ ConfirmMatch[
            CloudObject[ png, CloudObjectNameFormat -> "UUID" ],
            CloudObject[ _String, ___ ],
            "URI"
        ];

        "![Image]("<>uri<>")"
    ],
    throwInternalFailure
];

exportImage // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Notebook Tools*)

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*GetNotebookImage*)
$getNotebookImageDescription = "\
Captures an image of a notebook from the desktop Mathematica application. \
Requires the WolframNotebook palette to be running and listening on port 50000. \
Returns a markdown image link to the captured notebook screenshot.";

$defaultMCPTools[ "GetNotebookImage" ] := LLMTool @ <|
    "Name"        -> "GetNotebookImage",
    "DisplayName" -> "Get Notebook Image",
    "Description" -> $getNotebookImageDescription,
    "Function"    -> getNotebookImage,
    "Options"     -> { },
    "Parameters"  -> {
        "notebook" -> <|
            "Interpreter" -> "String",
            "Help"        -> "Which notebook to capture: 'InputNotebook' (default), 'EvaluationNotebook', 'SelectedNotebook', or part of a notebook title",
            "Required"    -> False
        |>,
        "resolution" -> <|
            "Interpreter" -> "Integer",
            "Help"        -> "Image resolution in DPI (default 144)",
            "Required"    -> False
        |>
    }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*getNotebookImage*)
getNotebookImage // beginDefinition;

getNotebookImage[ KeyValuePattern @ { "notebook" -> notebook_, "resolution" -> resolution_ } ] :=
    getNotebookImage[ notebook, resolution ];

getNotebookImage[ notebook_String, resolution_Integer ] :=
    sendNotebookCommand[ "GetNotebookImage", <| "notebook" -> notebook, "resolution" -> resolution |> ];

getNotebookImage[ _Missing, resolution_Integer ] :=
    getNotebookImage[ "InputNotebook", resolution ];

getNotebookImage[ notebook_String, _Missing ] :=
    getNotebookImage[ notebook, 144 ];

getNotebookImage[ _Missing, _Missing ] :=
    getNotebookImage[ "InputNotebook", 144 ];

getNotebookImage // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*NotebookCommand*)
$notebookCommandDescription = "\
Executes a command on the desktop Mathematica application via the WolframNotebook palette. \
Available commands: Ping, ListNotebooks, GetNotebookImage, SelectNotebook, EvaluateInNotebook, \
InsertCell, GetSelectedCells, DeleteCells, ReloadCommands, CreateNotebook, SaveNotebook, \
CloseNotebook, OpenNotebook, FrontEndToken. \
Pass command-specific parameters as JSON in the params field.";

$defaultMCPTools[ "NotebookCommand" ] := LLMTool @ <|
    "Name"        -> "NotebookCommand",
    "DisplayName" -> "Notebook Command",
    "Description" -> $notebookCommandDescription,
    "Function"    -> notebookCommand,
    "Options"     -> { },
    "Parameters"  -> {
        "command" -> <|
            "Interpreter" -> "String",
            "Help"        -> "Command name: Ping, ListNotebooks, GetNotebookImage, SelectNotebook, EvaluateInNotebook, InsertCell, GetSelectedCells, DeleteCells, ReloadCommands, CreateNotebook, SaveNotebook, CloseNotebook, OpenNotebook, FrontEndToken",
            "Required"    -> True
        |>,
        "notebook" -> <|
            "Interpreter" -> "String",
            "Help"        -> "Notebook to target (by title/filename), or 'InputNotebook' (default)",
            "Required"    -> False
        |>,
        "code" -> <|
            "Interpreter" -> "String",
            "Help"        -> "For EvaluateInNotebook: the code to evaluate",
            "Required"    -> False
        |>,
        "content" -> <|
            "Interpreter" -> "String",
            "Help"        -> "For InsertCell: the cell content",
            "Required"    -> False
        |>,
        "type" -> <|
            "Interpreter" -> "String",
            "Help"        -> "For InsertCell: cell type (Input, Text, Section, etc.)",
            "Required"    -> False
        |>,
        "resolution" -> <|
            "Interpreter" -> "Integer",
            "Help"        -> "For GetNotebookImage: image resolution in DPI (default 144)",
            "Required"    -> False
        |>,
        "path" -> <|
            "Interpreter" -> "String",
            "Help"        -> "For SaveNotebook/OpenNotebook: absolute file path",
            "Required"    -> False
        |>,
        "token" -> <|
            "Interpreter" -> "String",
            "Help"        -> "For FrontEndToken: the front end token to execute (e.g. 'Save', 'New', 'Copy')",
            "Required"    -> False
        |>,
        "title" -> <|
            "Interpreter" -> "String",
            "Help"        -> "For CreateNotebook: window title for new notebook",
            "Required"    -> False
        |>,
        "params" -> <|
            "Interpreter" -> "String",
            "Help"        -> "JSON object with additional command parameters (merged with explicit parameters)",
            "Required"    -> False
        |>
    }
|>;

notebookCommand // beginDefinition;

notebookCommand[ params_Association ] := Module[{ command, jsonParams, extraParams, cleanParams },
    command = Lookup[ params, "command", "Ping" ];

    (* Parse JSON params if provided *)
    jsonParams = Lookup[ params, "params", Missing[] ];
    extraParams = If[ StringQ[jsonParams],
        Quiet[ ImportString[ jsonParams, "RawJSON" ], {Import::jsonhintposandchar} ],
        <||>
    ];
    If[ !AssociationQ[extraParams], extraParams = <||> ];

    (* Merge: explicit params override JSON params, remove command and params keys *)
    cleanParams = DeleteCases[
        <| extraParams, KeyDrop[ params, {"command", "params"} ] |>,
        _Missing
    ];
    sendNotebookCommand[ command, cleanParams ]
];

notebookCommand // endDefinition;
(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*sendNotebookCommand*)
$notebookServerPort := With[{envPort = Environment["MCP_NOTEBOOK_PORT"]},
    If[StringQ[envPort], ToExpression[envPort], 50000]
];
$notebookReadTimeout = 30;
$notebookSocket = None;

(* Get or create persistent socket connection *)
getNotebookSocket // beginDefinition;

getNotebookSocket[] := Module[{},
    (* Check if existing socket is still valid *)
    If[MatchQ[$notebookSocket, _SocketObject] && Quiet[SocketReadyQ[$notebookSocket]] =!= $Failed,
        $notebookSocket,
        (* Close old socket if exists *)
        If[MatchQ[$notebookSocket, _SocketObject], Quiet[Close[$notebookSocket]]];
        (* Create new connection *)
        $notebookSocket = Quiet @ SocketConnect[{"127.0.0.1", $notebookServerPort}, "TCP"];
        $notebookSocket
    ]
];

getNotebookSocket // endDefinition;

sendNotebookCommand // beginDefinition;

sendNotebookCommand[ command_String, params_Association ] := Enclose[
    Catch @ Module[ { sock, request, requestJSON, response, result },

        (* Get persistent socket connection *)
        sock = getNotebookSocket[];
        If[ FailureQ @ sock || ! MatchQ[ sock, _SocketObject ],
            $notebookSocket = None;
            Throw[ "Error: Could not connect to desktop Mathematica on port " <> ToString @ $notebookServerPort <>
                   ". Is the WolframNotebook palette running and listening?" ]
        ];

        (* Build and send request *)
        request = <|
            "id"      -> CreateUUID[ ],
            "command" -> command,
            "params"  -> params
        |>;

        requestJSON = ConfirmBy[ ExportString[ request, "JSON" ], StringQ, "RequestJSON" ];

        (* Send request with newline delimiter *)
        WriteString[ sock, requestJSON <> "\n" ];

        (* Read response - wait for newline-terminated response *)
        response = ConfirmBy[
            TimeConstrained[ readSocketLine @ sock, $notebookReadTimeout, $TimedOut ],
            StringQ,
            "Response"
        ];

        (* Parse and format result *)
        result = ConfirmBy[ ImportString[ response, "RawJSON" ], AssociationQ, "Result" ];

        If[ result[ "status" ] === "error",
            Throw[ "Error from desktop: " <> ToString[ result[ "error" ] ] ]
        ];

        ConfirmBy[ formatNotebookResult[ command, result[ "result" ] ], StringQ, "FormattedResult" ]
    ],
    throwInternalFailure
];

sendNotebookCommand // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*readSocketLine*)
readSocketLine // beginDefinition;

readSocketLine[ sock_SocketObject ] := Module[ { accumulated = "", chunk, startTime = AbsoluteTime[] },
    (* Wait for initial data *)
    SocketWaitNext[ { sock }, Min[ 5, $notebookReadTimeout ] ];

    (* Keep reading until we get a newline-terminated response *)
    While[ AbsoluteTime[] - startTime < $notebookReadTimeout && !StringEndsQ[ accumulated, "\n" ],
        SocketWaitNext[ { sock }, 0.5 ];
        chunk = Quiet @ SocketReadMessage[ sock ];
        Which[
            chunk === EndOfFile, Break[],
            ByteArrayQ @ chunk, accumulated = accumulated <> ByteArrayToString[ chunk ],
            StringQ @ chunk, accumulated = accumulated <> chunk
        ]
    ];
    StringTrim @ accumulated
];

readSocketLine // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*formatNotebookResult*)
formatNotebookResult // beginDefinition;

(* Format based on result structure, not command name *)
formatNotebookResult[ command_String, result_Association ] := Module[
    { file, uri, output },

    (* If result has a file key pointing to an existing image, format as markdown image *)
    file = result[ "file" ];
    If[ StringQ[file] && FileExistsQ[file],
        uri = "file://" <> file;
        output = "![" <> command <> "](" <> uri <> ")";
        If[ KeyExistsQ[result, "width"] && KeyExistsQ[result, "height"],
            output = output <> "\n\nImage dimensions: " <>
                ToString[ result[ "width" ] ] <> "x" <> ToString[ result[ "height" ] ] <> " pixels"
        ];
        If[ KeyExistsQ[result, "count"],
            output = output <> "\nNotebook count: " <> ToString[ result[ "count" ] ]
        ];
        Return[ output ]
    ];

    (* Otherwise return as formatted association *)
    "Result: " <> ToString[ result, InputForm ]
];

formatNotebookResult // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*RAG Tools*)

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WolframContext*)
$defaultMCPTools[ "WolframContext" ] := LLMTool @ <|
    "Name"           -> "WolframContext",
    "DisplayName"    -> "Wolfram Context",
    "Description"    -> $wolframContextToolDescription,
    "Function"       -> relatedWolframContext,
    "LLMKit"         -> "Suggested",
    "Initialization" :> initializeVectorDatabases[ ],
    "Options"        -> { },
    "Parameters"     -> {
        "context" -> <|
            "Interpreter" -> "String",
            "Help"        -> "A detailed summary of what the user is trying to achieve or learn about.",
            "Required"    -> True
        |>
    }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*relatedWolframContext*)
relatedWolframContext // beginDefinition;

relatedWolframContext[ KeyValuePattern[ "context" -> context_ ] ] :=
    relatedWolframContext @ context;

relatedWolframContext[ context_String ] := Enclose[
    Module[ { waPrompt, wlPrompt },
        waPrompt = ConfirmBy[ relatedWolframAlphaPrompt[ context, "Warning" ], StringQ, "WolframAlphaPrompt" ];
        wlPrompt = ConfirmBy[ relatedDocumentation @ context, StringQ, "WolframLanguagePrompt" ];
        ConfirmBy[
            StringRiffle[ DeleteCases[ StringTrim @ { waPrompt, wlPrompt }, "" ], "\n\n======\n\n" ],
            StringQ,
            "Result"
        ]
    ],
    throwInternalFailure
];

relatedWolframContext // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*relatedWolframAlphaPrompt*)
relatedWolframAlphaPrompt // beginDefinition;

relatedWolframAlphaPrompt[ context_ ] :=
    relatedWolframAlphaPrompt[ context, "Error" ];

relatedWolframAlphaPrompt[ context_, level_ ] :=
    relatedWolframAlphaPrompt[ context, level, llmKitSubscribedQ[ ] ];

relatedWolframAlphaPrompt[ context_, level_, True ] :=
    relatedWolframAlphaResults @ context;

relatedWolframAlphaPrompt[ context_, level_, False ] := Enclose[
    Module[ { info, url, connected, template },
        info      = ConfirmBy[ getLLMKitInfo[ ], AssociationQ, "LLMKitInfo" ];
        url       = ConfirmBy[ info[ "buyNowUrl" ], StringQ, "BuyNowURL" ];
        connected = ConfirmBy[ info[ "connected" ], BooleanQ, "Connected" ];
        template  = If[ connected, $wolframAlphaMissingLLMKitTemplate, $wolframAlphaNoCloudTemplate ];
        ConfirmBy[ TemplateApply[ template, <| "URL" -> url, "Level" -> level |> ], StringQ, "Result" ]
    ],
    throwInternalFailure
];

relatedWolframAlphaPrompt // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*relatedWolframAlphaResults*)
relatedWolframAlphaResults // beginDefinition;

relatedWolframAlphaResults[ KeyValuePattern[ "context" -> context_ ] ] :=
    relatedWolframAlphaResults @ context;

relatedWolframAlphaResults[ context_String ] := Enclose[
    Module[ { prompt },
        ConfirmMatch[ chatbookVersionCheck[ ], True, "ChatbookVersionCheck" ];
        prompt = ConfirmBy[ cb`RelatedWolframAlphaResults[ context, "Prompt" ], StringQ, "Prompt" ];
        StringTrim @ prompt
    ],
    throwInternalFailure
];

relatedWolframAlphaResults // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WolframAlphaContext*)
$defaultMCPTools[ "WolframAlphaContext" ] := LLMTool @ <|
    "Name"           -> "WolframAlphaContext",
    "DisplayName"    -> "Wolfram|Alpha Context",
    "Description"    -> $waContextToolDescription,
    "Function"       -> relatedWolframAlphaPrompt,
    "LLMKit"         -> "Required",
    "Initialization" :> initializeVectorDatabases[ ],
    "Options"        -> { },
    "Parameters"     -> {
        "context" -> <|
            "Interpreter" -> "String",
            "Help"        -> "A detailed summary of what the user is trying to achieve or learn about.",
            "Required"    -> True
        |>
    }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WolframLanguageContext*)
$defaultMCPTools[ "WolframLanguageContext" ] := LLMTool @ <|
    "Name"           -> "WolframLanguageContext",
    "DisplayName"    -> "Wolfram Language Context",
    "Description"    -> $wlContextToolDescription,
    "Function"       -> relatedDocumentation,
    "LLMKit"         -> "Suggested",
    "Initialization" :> initializeVectorDatabases[ ],
    "Options"        -> { },
    "Parameters"     -> {
        "context" -> <|
            "Interpreter" -> "String",
            "Help"        -> "A detailed summary of what the user is trying to achieve or learn about.",
            "Required"    -> True
        |>
    }
|>;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*relatedDocumentation*)
relatedDocumentation // beginDefinition;

relatedDocumentation[ KeyValuePattern[ "context" -> context_ ] ] :=
    relatedDocumentation @ context;

relatedDocumentation[ context_String ] := Enclose[
    Module[ { prompt, formatted },

        ConfirmMatch[ chatbookVersionCheck[ ], True, "ChatbookVersionCheck" ];

        prompt = ConfirmBy[ relatedDocumentation0 @ context, StringQ, "Prompt" ];

        formatted = If[ StringTrim @ prompt === "",
                        "",
                        $documentationPromptHeader <> formatDocumentationSnippets @ prompt
                    ];

        ConfirmBy[ formatted, StringQ, "Result" ]
    ],
    throwInternalFailure
];

relatedDocumentation // endDefinition;


relatedDocumentation0 // beginDefinition;

relatedDocumentation0[ context_ ] :=
    relatedDocumentation0[ context, llmKitSubscribedQ[ ] ];

relatedDocumentation0[ context_, True ] :=
    Block[ { $EvaluationEnvironment = "Script" },
        cb`RelatedDocumentation[ context, "Prompt", "PromptHeader" -> False, "FilterResults" -> True, MaxItems -> 50 ]
    ];

relatedDocumentation0[ context_, False ] :=
    Block[ { $EvaluationEnvironment = "Script" },
        cb`RelatedDocumentation[ context, "Prompt", "PromptHeader" -> False, "FilterResults" -> False, MaxItems -> 10 ]
    ];

relatedDocumentation0 // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*formatDocumentationSnippets*)
formatDocumentationSnippets // beginDefinition;

formatDocumentationSnippets[ s_String ] := Enclose[
    Module[ { string },
        string = ConfirmBy[
            If[ StringContainsQ[ s, "\n\n======\n\n" ],
                formatDocumentationSnippets @ StringSplit[ s, "\n======\n" ],
                s
            ],
            StringQ,
            "String"
        ];

        StringReplace[
            string,
            {
                Shortest[ "\\!\\(\\*MarkdownImageBox[\"![" ~~ label: Except[ "]" ]... ~~ "](" ~~ __ ~~ ")\"]\\)" ] :>
                    "Image[...]",

                Shortest[ "[" ~~ label: Except[ "]" ]... ~~ "](paclet:" ~~ uri: Except[ ")" ].. ~~ ")" ] :>
                    "["<>label<>"](https://reference.wolfram.com/language/"<>uri<>")"
            }
        ]
    ],
    throwInternalFailure
];

formatDocumentationSnippets[ snippets: { __String } ] := Enclose[
	StringRiffle[
        ConfirmMatch[ toSnippetString /@ snippets, { __String }, "SnippetStrings" ],
        "\n\n"
    ],
    throwInternalFailure
];

formatDocumentationSnippets // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toSnippetString*)
toSnippetString // beginDefinition;

toSnippetString[ snippet_String ] :=
    toSnippetString @ StringSplit[ StringTrim @ snippet, s: "\n".. :> s ];

toSnippetString[ { header_, "\n", uri0_String, rest___String } ] /; StringContainsQ[ uri0, ":" ] := Enclose[
    Module[ { uri, text },
        uri  = ConfirmBy[ toDocumentationURL @ uri0, StringQ, "URI" ];
        text = ConfirmBy[ header <> "\n\n" <> StringTrim @ StringJoin @ rest, StringQ, "Text" ];
        ConfirmBy[ TemplateApply[ $snippetTemplate, <| "URI" -> uri, "Text" -> text |> ], StringQ, "Result" ]
    ],
    throwInternalFailure
];

toSnippetString[ { other__String } ] :=
    StringTrim @ StringJoin @ other;

toSnippetString // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Subsubsection::Closed:: *)
(*toDocumentationURL*)
toDocumentationURL // beginDefinition;

toDocumentationURL[ uri_String ] := StringReplace[
    uri,
    StartOfString~~"paclet:" -> "https://reference.wolfram.com/language/"
];

toDocumentationURL // endDefinition;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*$DefaultMCPServers*)
$DefaultMCPServers := WithCleanup[
    Unprotect @ $DefaultMCPServers,
    $DefaultMCPServers = MCPServerObject /@ AssociationMap[ Apply @ Rule, $defaultMCPServers ],
    Protect @ $DefaultMCPServers
];

$defaultMCPServers = <| |>;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*Wolfram*)
$defaultMCPServers[ "Wolfram" ] := <|
    "Name"          -> "Wolfram",
    "Location"      -> "BuiltIn",
    "Transport"     -> "StandardInputOutput",
    "ServerVersion" -> $pacletVersion,
    "ObjectVersion" -> $objectVersion,
    "LLMEvaluator"  -> <|
        "Tools" -> {
            "WolframContext",
            "WolframLanguageEvaluator",
            "WolframAlpha"
        },
        "MCPPrompts" -> { "WolframSearch" }
    |>
|>;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WolframAlpha*)
$defaultMCPServers[ "WolframAlpha" ] := <|
    "Name"          -> "WolframAlpha",
    "Location"      -> "BuiltIn",
    "Transport"     -> "StandardInputOutput",
    "ServerVersion" -> $pacletVersion,
    "ObjectVersion" -> $objectVersion,
    "LLMEvaluator"  -> <|
        "Tools" -> {
            "WolframAlphaContext",
            "WolframAlpha"
        },
        "MCPPrompts" -> { "WolframAlphaSearch" }
    |>
|>;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WolframLanguage*)
$defaultMCPServers[ "WolframLanguage" ] := <|
    "Name"          -> "WolframLanguage",
    "Location"      -> "BuiltIn",
    "Transport"     -> "StandardInputOutput",
    "ServerVersion" -> $pacletVersion,
    "ObjectVersion" -> $objectVersion,
    "LLMEvaluator"  -> <|
        "Tools" -> {
            "WolframLanguageContext",
            "WolframLanguageEvaluator",
            "ReadNotebook",
            "WriteNotebook",
            "SymbolDefinition",
            "TestReport"
        },
        "MCPPrompts" -> { "WolframLanguageSearch", "Notebook" }
    |>
|>;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WolframPacletDevelopment*)
$defaultMCPServers[ "WolframPacletDevelopment" ] := <|
    "Name"          -> "WolframPacletDevelopment",
    "Location"      -> "BuiltIn",
    "Transport"     -> "StandardInputOutput",
    "ServerVersion" -> $pacletVersion,
    "ObjectVersion" -> $objectVersion,
    "LLMEvaluator"  -> <|
        "Tools" -> {
            "WolframLanguageContext",
            "WolframLanguageEvaluator",
            "ReadNotebook",
            "WriteNotebook",
            "SymbolDefinition",
            "TestReport",
            "CreateSymbolDoc",
            "EditSymbolDoc",
            "EditSymbolDocExamples"
        },
        "MCPPrompts" -> { "WolframLanguageSearch", "Notebook" }
    |>
|>;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WolframAll*)
$defaultMCPServers[ "WolframAll" ] := <|
    "Name"          -> "WolframAll",
    "Location"      -> "BuiltIn",
    "Transport"     -> "StandardInputOutput",
    "ServerVersion" -> $pacletVersion,
    "ObjectVersion" -> $objectVersion,
    "LLMEvaluator"  -> <|
        "Tools" -> {
            "WolframContext",
            "WolframLanguageContext",
            "WolframAlphaContext",
            "WolframLanguageEvaluator",
            "WolframAlpha",
            "ReadNotebook",
            "WriteNotebook",
            "NotebookCommand"
        }
    |>
|>;

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
(*WolframNotebook*)
$defaultMCPServers[ "WolframNotebook" ] := <|
    "Name"          -> "WolframNotebook",
    "Location"      -> "BuiltIn",
    "Transport"     -> "StandardInputOutput",
    "ServerVersion" -> $pacletVersion,
    "ObjectVersion" -> $objectVersion,
    "LLMEvaluator"  -> <|
        "Tools" -> {
            "NotebookCommand"
        }
    |>
|>;

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Footer*)
addToMXInitialization[
    $DefaultMCPServers
];

End[ ];
EndPackage[ ];
