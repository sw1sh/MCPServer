(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Package Header*)
BeginPackage[ "Wolfram`MCPServer`Tools`NotebookCommand`" ];
Begin[ "`Private`" ];

Needs[ "Wolfram`MCPServer`"        ];
Needs[ "Wolfram`MCPServer`Common`" ];
Needs[ "Wolfram`MCPServer`Tools`"  ];

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Tool Definitions*)

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
(* ::Subsection::Closed:: *)
(*NotebookCommand*)
$notebookCommandDescription = "\
Executes a command on the desktop Mathematica application via the WolframNotebook palette. \
Available commands: Ping, ListNotebooks, GetNotebookImage, GetAllNotebooksImage, CaptureScreen, \
SelectNotebook, EvaluateInNotebook, InsertCell, GetSelectedCells, DeleteCells, ReloadCommands, \
CreateNotebook, SaveNotebook, CloseNotebook, OpenNotebook, FrontEndToken, FocusWolframDesktop, \
ListButtons, ClickButton, MouseClick, SendKeys.

IMPORTANT WORKFLOW FOR DESKTOP INTERACTION:
1. ALWAYS start with CaptureScreen or ListNotebooks to see current state
2. Use GetNotebookImage to see a specific notebook's content
3. Use FocusWolframDesktop to bring Wolfram to foreground before clicking
4. Use ListButtons to get button positions in a palette BEFORE clicking
5. Use ClickButton with 'target' param to specify where palette inserts content
6. Use SendKeys for keyboard input (text characters)

COMMAND REFERENCE:
- CaptureScreen: Full desktop screenshot
- ListNotebooks: List all open notebooks
- GetNotebookImage(notebook, resolution): Screenshot of specific notebook
- GetAllNotebooksImage(resolution): Collage of all notebooks
- FocusWolframDesktop: Bring Wolfram to front (required before clicking)
- ListButtons(notebook): Get button positions in a notebook/palette
- ClickButton(notebook, index, target): Click button by index, target=notebook for insertion
- SendKeys(text, notebook): Send keyboard input
- EvaluateInNotebook(notebook, code): Evaluate code in a notebook
- SelectNotebook(notebook): Set a notebook as selected

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

(* ::**************************************************************************************************************:: *)
(* ::Section::Closed:: *)
(*Definitions*)

(* ::**************************************************************************************************************:: *)
(* ::Subsection::Closed:: *)
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
(*notebookCommand*)
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
(* ::Subsection::Closed:: *)
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
(* ::Subsection::Closed:: *)
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
(* ::Subsection::Closed:: *)
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
(*Package Footer*)
End[ ];
EndPackage[ ];
