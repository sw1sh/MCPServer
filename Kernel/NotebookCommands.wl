(* ::Package:: *)
(* ::Section::Closed:: *)
(*NotebookCommands*)

(* Notebook command handlers for WolframNotebook server *)
(* These run in the desktop Mathematica frontend kernel *)

BeginPackage["Wolfram`MCPServer`NotebookCommands`"];

ExecuteNotebookCommand::usage = "ExecuteNotebookCommand[command, params] executes a notebook command and returns a result association.";

Begin["`Private`"];

(* ::Subsection::Closed:: *)
(*Helper Functions*)

(* Find notebook by title or filename *)
findNotebook[spec_String] := Module[{nbs, matchTitle, matchId},
    nbs = Notebooks[];
    (* Try matching window title first *)
    matchTitle = Select[nbs, StringContainsQ[ToString[Quiet[CurrentValue[#, WindowTitle]]], spec, IgnoreCase -> True] &];
    If[Length[matchTitle] > 0, Return[First[matchTitle]]];
    (* Try matching notebook ID (includes filename) *)
    matchId = Select[nbs, StringContainsQ[ToString[#], spec, IgnoreCase -> True] &];
    If[Length[matchId] > 0, First[matchId], $Failed]
];

(* Get notebook from spec *)
getNotebook[spec_String] := Switch[spec,
    "InputNotebook", InputNotebook[],
    "EvaluationNotebook", EvaluationNotebook[],
    "SelectedNotebook", SelectedNotebook[],
    _, findNotebook[spec]
];

(* Safe title extraction *)
safeTitle[nb_NotebookObject] := Module[{title = Quiet[CurrentValue[nb, WindowTitle]]},
    If[StringQ[title], title, "Untitled"]
];

(* ::Subsection::Closed:: *)
(*Command Handlers*)

ExecuteNotebookCommand["Ping", _] := <|"pong" -> True, "timestamp" -> DateString[]|>;

ExecuteNotebookCommand["Version", _] := <|"version" -> "1.1.0", "reloadTest" -> "Hot-reload works!"|>;

ExecuteNotebookCommand["ListNotebooks", _] := <|
    "notebooks" -> Map[
        <|"id" -> ToString[#], "title" -> safeTitle[#]|> &,
        Notebooks[]
    ]
|>;

(* Helper to save image to temp file and return path *)
saveImageToTemp[img_Image, prefix_String] := Module[
    {hash, dir, file},
    hash = Hash[ImageData[img], Automatic, "HexString"];
    dir = FileNameJoin[{$TemporaryDirectory, "MCPNotebookImages"}];
    If[!DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
    file = FileNameJoin[{dir, prefix <> "_" <> StringTake[hash, 16] <> ".png"}];
    Export[file, img, "PNG"];
    file
];

ExecuteNotebookCommand["GetNotebookImage", params_Association] := Module[
    {spec, resolution, nb, img, dims, title, file},
    spec = Lookup[params, "notebook", "InputNotebook"];
    resolution = Lookup[params, "resolution", 144];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Could not find notebook"|>]];
    img = Rasterize[nb, ImageResolution -> resolution];
    If[!ImageQ[img], Return[<|"error" -> "Failed to rasterize notebook"|>]];
    dims = ImageDimensions[img];
    title = safeTitle[nb];
    file = saveImageToTemp[img, "notebook"];
    <|
        "file" -> file,
        "width" -> dims[[1]],
        "height" -> dims[[2]],
        "title" -> title
    |>
];

ExecuteNotebookCommand["GetAllNotebooksImage", params_Association] := Module[
    {resolution, nbs, images, combined, dims, file},
    resolution = Lookup[params, "resolution", 72];
    nbs = Notebooks[];
    images = Map[
        Function[nb, Quiet[Rasterize[nb, ImageResolution -> resolution]]],
        nbs
    ];
    images = Select[images, ImageQ];
    If[Length[images] === 0, Return[<|"error" -> "No notebooks to capture"|>]];

    (* Combine into collage *)
    combined = If[Length[images] === 1,
        First[images],
        ImageCollage[images,
            Background -> GrayLevel[0.2],
            ImagePadding -> 10,
            ImageSize -> {1600, Automatic}
        ]
    ];
    If[!ImageQ[combined], Return[<|"error" -> "Failed to create collage"|>]];
    dims = ImageDimensions[combined];
    file = saveImageToTemp[combined, "allnotebooks"];
    <|
        "file" -> file,
        "width" -> dims[[1]],
        "height" -> dims[[2]],
        "count" -> Length[images]
    |>
];

ExecuteNotebookCommand["CaptureScreen", params_Association] := Module[
    {tempFile, result, img, dims, file},
    tempFile = FileNameJoin[{$TemporaryDirectory, "screen_capture_" <> ToString[RandomInteger[10^9]] <> ".png"}];
    result = RunProcess[{"screencapture", "-x", tempFile}];
    If[result["ExitCode"] =!= 0, Return[<|"error" -> "screencapture failed"|>]];
    If[!FileExistsQ[tempFile], Return[<|"error" -> "Screenshot file not created"|>]];
    img = Import[tempFile];
    DeleteFile[tempFile];
    If[!ImageQ[img], Return[<|"error" -> "Failed to import screenshot"|>]];
    dims = ImageDimensions[img];
    file = saveImageToTemp[img, "screen"];
    <|
        "file" -> file,
        "width" -> dims[[1]],
        "height" -> dims[[2]]
    |>
];

ExecuteNotebookCommand["SelectNotebook", params_Association] := Module[{spec, nb},
    spec = Lookup[params, "notebook", ""];
    nb = findNotebook[spec];
    If[MatchQ[nb, _NotebookObject],
        SetSelectedNotebook[nb];
        <|"success" -> True, "notebook" -> ToString[nb]|>,
        <|"error" -> "Notebook not found: " <> spec|>
    ]
];

ExecuteNotebookCommand["EvaluateInNotebook", params_Association] := Module[
    {spec, code, nb, cell},
    spec = Lookup[params, "notebook", "InputNotebook"];
    code = Lookup[params, "code", ""];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];
    (* Insert cell, select it, evaluate *)
    cell = Cell[BoxData[code], "Input"];
    NotebookWrite[nb, cell];
    SelectionMove[nb, Previous, Cell];
    SelectionEvaluate[nb];
    <|"success" -> True, "code" -> code|>
];

ExecuteNotebookCommand["InsertCell", params_Association] := Module[
    {spec, content, cellType, nb, cell},
    spec = Lookup[params, "notebook", "InputNotebook"];
    content = Lookup[params, "content", ""];
    cellType = Lookup[params, "type", "Input"];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];
    cell = Cell[content, cellType];
    NotebookWrite[nb, cell];
    <|"success" -> True|>
];

ExecuteNotebookCommand["GetSelectedCells", params_Association] := Module[
    {spec, nb, cells},
    spec = Lookup[params, "notebook", "InputNotebook"];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];
    cells = NotebookRead[nb];
    <|"cells" -> If[MatchQ[cells, _Cell | {__Cell}],
        Map[<|"type" -> #[[2]], "content" -> ToString[#[[1]]]|> &, Flatten[{cells}]],
        {}
    ]|>
];

ExecuteNotebookCommand["DeleteCells", params_Association] := Module[{spec, nb},
    spec = Lookup[params, "notebook", "InputNotebook"];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];
    NotebookDelete[nb];
    <|"success" -> True|>
];

ExecuteNotebookCommand["ReloadCommands", _] := Module[{},
    (* Reload this package *)
    Quiet[Get["Wolfram`MCPServer`NotebookCommands`"]];
    <|"success" -> True, "message" -> "Commands reloaded"|>
];

(* ::Subsection::Closed:: *)
(*Notebook Creation and Management*)

ExecuteNotebookCommand["CreateNotebook", params_Association] := Module[
    {nb, title},
    nb = CreateDocument[];
    title = Lookup[params, "title", Missing[]];
    If[StringQ[title], SetOptions[nb, WindowTitle -> title]];
    <|"success" -> True, "notebook" -> ToString[nb]|>
];

ExecuteNotebookCommand["SaveNotebook", params_Association] := Module[
    {spec, path, nb, savedPath},
    spec = Lookup[params, "notebook", "InputNotebook"];
    path = Lookup[params, "path", Missing[]];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];
    If[StringQ[path],
        NotebookSave[nb, path];
        savedPath = path,
        NotebookSave[nb];
        savedPath = Quiet[NotebookFileName[nb]];
        If[!StringQ[savedPath], savedPath = "Saved (no path)"]
    ];
    <|"success" -> True, "path" -> savedPath|>
];

ExecuteNotebookCommand["CloseNotebook", params_Association] := Module[
    {spec, nb},
    spec = Lookup[params, "notebook", "InputNotebook"];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];
    NotebookClose[nb];
    <|"success" -> True|>
];

ExecuteNotebookCommand["OpenNotebook", params_Association] := Module[
    {path, nb},
    path = Lookup[params, "path", ""];
    If[!StringQ[path] || path === "", Return[<|"error" -> "Path required"|>]];
    If[!FileExistsQ[path], Return[<|"error" -> "File not found: " <> path|>]];
    nb = NotebookOpen[path];
    If[MatchQ[nb, _NotebookObject],
        <|"success" -> True, "notebook" -> ToString[nb]|>,
        <|"error" -> "Failed to open notebook"|>
    ]
];

(* ::Subsection::Closed:: *)
(*Front End Commands*)

ExecuteNotebookCommand["FrontEndToken", params_Association] := Module[
    {token, spec, nb, param},
    token = Lookup[params, "token", ""];
    spec = Lookup[params, "notebook", Missing[]];
    param = Lookup[params, "param", Missing[]];
    If[token === "", Return[<|"error" -> "Token required"|>]];

    nb = If[MissingQ[spec], InputNotebook[], getNotebook[spec]];

    Which[
        MissingQ[param],
        FrontEndTokenExecute[nb, token],
        True,
        FrontEndTokenExecute[nb, token, param]
    ];
    <|"success" -> True, "token" -> token|>
];

ExecuteNotebookCommand["CreateFunctionResourceNotebook", params_Association] := Module[
    {nb, funcName},
    funcName = Lookup[params, "name", "MyFunction"];
    (* Use FrontEndToken to create a Function Repository Item notebook *)
    FrontEndTokenExecute["NewFunctionResourceNotebook"];
    Pause[0.5]; (* Give time for notebook to open *)
    nb = InputNotebook[];
    <|"success" -> True, "notebook" -> ToString[nb], "note" -> "Created Function Resource notebook template"|>
];

(* ::Subsection::Closed:: *)
(*Button and Mouse Commands*)

(* Helper to bring Wolfram Desktop to front using macOS osascript *)
focusWolframDesktop[] := Module[{result},
    result = RunProcess[{
        "osascript", "-e",
        "tell application \"System Events\" to set frontmost of process \"WolframNB\" to true"
    }];
    result["ExitCode"] === 0
];

ExecuteNotebookCommand["FocusWolframDesktop", params_Association] := Module[{success},
    success = focusWolframDesktop[];
    If[success,
        <|"success" -> True, "message" -> "Mathematica brought to front"|>,
        <|"error" -> "Failed to activate Mathematica"|>
    ]
];

ExecuteNotebookCommand["ListButtons", params_Association] := Module[
    {spec, nb, positions, getBoxPositions},
    spec = Lookup[params, "notebook", "InputNotebook"];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    (* Use ResourceFunction to get button positions *)
    getBoxPositions = ResourceFunction["GetBoxPositions"];
    positions = Quiet[getBoxPositions[nb, "ButtonBox"]];

    If[!MatchQ[positions, _Association | _List],
        Return[<|"error" -> "Failed to get button positions"|>]
    ];

    <|
        "success" -> True,
        "notebook" -> ToString[nb],
        "buttons" -> If[AssociationQ[positions], positions["ButtonBox"], positions]
    |>
];

ExecuteNotebookCommand["ClickButton", params_Association] := Module[
    {spec, nb, target, targetNb, index, x, y, positions, pos, getBoxPositions, moveMouse},
    spec = Lookup[params, "notebook", "InputNotebook"];
    target = Lookup[params, "target", Missing[]]; (* Target notebook for insertion *)
    index = Lookup[params, "index", 1];
    x = Lookup[params, "x", Missing[]];
    y = Lookup[params, "y", Missing[]];

    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    getBoxPositions = ResourceFunction["GetBoxPositions"];
    moveMouse = ResourceFunction["MoveMouse"];

    (* If x,y provided, use those; otherwise get from button index *)
    If[NumericQ[x] && NumericQ[y],
        pos = {x, y},
        (* Get button positions and use index *)
        positions = Quiet[getBoxPositions[nb, "ButtonBox"]];
        If[!MatchQ[positions, _Association | _List],
            Return[<|"error" -> "Failed to get button positions"|>]
        ];
        positions = If[AssociationQ[positions], positions["ButtonBox"], positions];
        If[!ListQ[positions] || Length[positions] < index,
            Return[<|"error" -> "Button index out of range"|>]
        ];
        pos = positions[[index]]
    ];

    (* If target specified, select it first (where palette will insert content) *)
    If[StringQ[target],
        targetNb = getNotebook[target];
        If[MatchQ[targetNb, _NotebookObject], SetSelectedNotebook[targetNb]]
    ];

    (* Bring Wolfram Desktop to front using osascript, then click *)
    (* GetBoxPositions returns screen absolute coords, so use All scope *)
    focusWolframDesktop[];
    Pause[0.3]; (* Delay for window to come to front *)
    moveMouse[All, pos, "Click"];

    <|"success" -> True, "clicked" -> pos, "target" -> If[StringQ[target], target, "none"]|>
];

ExecuteNotebookCommand["MouseClick", params_Association] := Module[
    {x, y, scope, moveMouse},
    x = Lookup[params, "x", Missing[]];
    y = Lookup[params, "y", Missing[]];
    scope = Lookup[params, "scope", "All"]; (* "All" for screen coordinates *)

    If[!NumericQ[x] || !NumericQ[y],
        Return[<|"error" -> "x and y coordinates required"|>]
    ];

    moveMouse = ResourceFunction["MoveMouse"];

    (* Click at coordinates *)
    moveMouse[If[scope === "All", All, InputNotebook[]], {x, y}, "Click"];

    <|"success" -> True, "clicked" -> {x, y}, "scope" -> scope|>
];

(* Default handler for unknown commands *)
ExecuteNotebookCommand[cmd_, _] := <|"error" -> "Unknown command: " <> ToString[cmd]|>;

End[];
EndPackage[];
