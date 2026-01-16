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
    (* Use CurrentNotebookImage for full window capture including docked cells *)
    img = CurrentNotebookImage[nb, ImageResolution -> resolution];
    If[!ImageQ[img], Return[<|"error" -> "Failed to capture notebook image"|>]];
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
    result = RunProcess[{"screencapture", "-x", "-C", tempFile}]; (* -C shows cursor *)
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

(* Evaluate code in background without creating cells *)
ExecuteNotebookCommand["Evaluate", params_Association] := Module[
    {code, result},
    code = Lookup[params, "code", ""];
    If[!StringQ[code] || code === "",
        Return[<|"error" -> "code parameter required"|>]
    ];
    result = ToExpression[code];
    <|"success" -> True, "result" -> ToString[result, InputForm]|>
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

ExecuteNotebookCommand[cmd_String, params_String] :=
    ExecuteNotebookCommand[cmd, ImportString[params, "RawJSON"]];

(* Create a Function Resource notebook template *)
ExecuteNotebookCommand["CreateFunctionResourceNotebook", params_Association] := Module[
    {nb, funcName},
    funcName = Lookup[params, "name", "MyFunction"];
    (* Use CreateNotebook to create a Function Repository Item notebook *)
    nb = CreateNotebook["FunctionResource"];
    If[!MatchQ[nb, _NotebookObject],
        Return[<|"error" -> "Failed to create Function Resource notebook"|>]
    ];
    <|"success" -> True, "notebook" -> ToString[nb], "note" -> "Created Function Resource notebook template"|>
];

(* ::Subsection::Closed:: *)
(*Notebook Editing Commands*)

(* Scroll a notebook to a specific position *)
ExecuteNotebookCommand["ScrollNotebook", params_Association] := Module[
    {spec, position, nb},
    spec = Lookup[params, "notebook", "InputNotebook"];
    position = Lookup[params, "position", "Top"]; (* Top, Bottom, or cell index *)
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    Which[
        position === "Top",
        SelectionMove[nb, Before, Notebook];
        FrontEndExecute[FrontEnd`SelectionMove[nb, Before, Notebook]],
        position === "Bottom",
        SelectionMove[nb, After, Notebook];
        FrontEndExecute[FrontEnd`SelectionMove[nb, After, Notebook]],
        IntegerQ[position],
        (* Move to specific cell index *)
        SelectionMove[nb, Before, Notebook];
        Do[SelectionMove[nb, Next, Cell], position];
        FrontEndExecute[FrontEnd`FrontEndToken[nb, "ScrollNotebookToSelection"]]
    ];
    <|"success" -> True, "position" -> position|>
];

(* Get the structure of a notebook - list of cell styles and contents *)
ExecuteNotebookCommand["GetNotebookStructure", params_Association] := Module[
    {spec, nb, cells, structure, styleStr},
    spec = Lookup[params, "notebook", "InputNotebook"];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    cells = Cells[nb];
    structure = MapIndexed[
        Function[{cell, idx},
            styleStr = Quiet[CurrentValue[cell, CellStyle]];
            styleStr = If[ListQ[styleStr], First[styleStr, "Unknown"], ToString[styleStr]];
            <|
                "index" -> First[idx],
                "style" -> styleStr,
                "preview" -> StringTake[
                    ToString[Quiet[NotebookRead[cell]] /. Cell[x_, ___] :> x],
                    UpTo[80]
                ]
            |>
        ],
        cells
    ];
    <|"success" -> True, "cellCount" -> Length[cells], "cells" -> structure|>
];

(* Find a cell by its style and optionally by index within that style *)
ExecuteNotebookCommand["FindCellByStyle", params_Association] := Module[
    {spec, style, occurrence, nb, cells, matching, target},
    spec = Lookup[params, "notebook", "InputNotebook"];
    style = Lookup[params, "style", "Input"];
    occurrence = Lookup[params, "occurrence", 1]; (* Which occurrence of this style *)
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    cells = Cells[nb];
    matching = Select[cells, MemberQ[Flatten[{Quiet[CurrentValue[#, CellStyle]]}], style] &];

    If[Length[matching] < occurrence,
        Return[<|"error" -> "Cell not found", "found" -> Length[matching], "requested" -> occurrence|>]
    ];

    target = matching[[occurrence]];
    SetSelectedNotebook[nb];
    SelectionMove[target, All, Cell];

    <|
        "success" -> True,
        "cell" -> ToString[target],
        "totalMatching" -> Length[matching],
        "preview" -> StringTake[ToString[Quiet[NotebookRead[target]] /. Cell[x_, ___] :> x], UpTo[100]]
    |>
];

(* Set the content of the currently selected cell *)
ExecuteNotebookCommand["SetCellContent", params_Association] := Module[
    {spec, content, cellStyle, nb},
    spec = Lookup[params, "notebook", "InputNotebook"];
    content = Lookup[params, "content", ""];
    cellStyle = Lookup[params, "style", Automatic];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    (* Delete current selection and write new content *)
    NotebookDelete[nb];
    If[cellStyle === Automatic,
        NotebookWrite[nb, content],
        NotebookWrite[nb, Cell[content, cellStyle]]
    ];
    <|"success" -> True|>
];

(* Replace content in a cell found by style *)
ExecuteNotebookCommand["ReplaceCellByStyle", params_Association] := Module[
    {spec, style, occurrence, content, cellStyle, nb, cells, matching, target},
    spec = Lookup[params, "notebook", "InputNotebook"];
    style = Lookup[params, "style", "Input"];
    occurrence = Lookup[params, "occurrence", 1];
    content = Lookup[params, "content", ""];
    cellStyle = Lookup[params, "newStyle", style]; (* Keep same style by default *)
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    matching = Cells[nb, CellStyle -> style];

    If[Length[matching] < occurrence,
        Return[<|"error" -> "Cell not found", "found" -> Length[matching]|>]
    ];

    target = matching[[occurrence]];
    SelectionMove[target, All, Cell];
    NotebookDelete[nb];
    NotebookWrite[nb, Cell[content, cellStyle]];

    <|"success" -> True, "replaced" -> style, "occurrence" -> occurrence|>
];

(* Update the content of a cell without deleting the cell itself - preserves styling *)
(* Update the content of a cell without deleting the cell itself - preserves styling and options *)
ExecuteNotebookCommand["UpdateCellContentsByStyle", params_Association] := Module[
    {spec, style, occurrence, textData, nb, matching, target, newContent},
    spec = Lookup[params, "notebook", "InputNotebook"];
    style = Lookup[params, "style", "Title"];
    occurrence = Lookup[params, "occurrence", 1];
    textData = Lookup[params, "textData", ""];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    matching = Cells[nb, CellStyle -> style];

    If[Length[matching] < occurrence,
        Return[<|"error" -> "Cell not found", "found" -> Length[matching]|>]
    ];

    target = matching[[occurrence]];

    (* Select content inner structure to bypass Deletable->False locks on Cell object *)
    SelectionMove[target, All, CellContents];

    (* Write new content *)
    NotebookWrite[nb, textData];

    (* Verify update by reading back the cell *)
    newContent = NotebookRead[target];

    <|
        "success" -> True,
        "updated" -> style,
        "occurrence" -> occurrence,
        "verification" -> ToString[newContent, InputForm]
    |>
];

(* Get the content of a specific cell by style for inspection *)
ExecuteNotebookCommand["GetCellContent", params_Association] := Module[
    {spec, style, occurrence, nb, cells, matching, target, content},
    spec = Lookup[params, "notebook", "InputNotebook"];
    style = Lookup[params, "style", "Title"];
    occurrence = Lookup[params, "occurrence", 1];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    matching = Cells[nb, CellStyle -> style];

    If[Length[matching] < occurrence,
        Return[<|"error" -> "Cell not found", "found" -> Length[matching]|>]
    ];

    target = matching[[occurrence]];
    content = NotebookRead[target];

    <|
        "success" -> True,
        "style" -> style,
        "occurrence" -> occurrence,
        "content" -> ToString[content, InputForm]
    |>
];

ExecuteNotebookCommand["WhyTheBeep", _] := Module[{nb, imgInfo},
    (* Use the correct FrontEnd token for Why the Beep dialog *)
    FrontEndTokenExecute["ExplainBeepDialog"];
    Pause[0.5];
    nb = First[Select[Notebooks[], StringMatchQ[CurrentValue[#, WindowTitle], "*Beep*", IgnoreCase -> True] &], $Failed];
    If[FailureQ[nb], Return[<|"error" -> "No beep explanation window found (no recent beep occurred)"|>]];

    imgInfo = ExecuteNotebookCommand["GetNotebookImage", <|"notebook" -> CurrentValue[nb, WindowTitle], "resolution" -> 144|>];
    NotebookClose[nb];
    imgInfo
];

ExecuteNotebookCommand["FillFunctionResourceNotebook", params_Association] := Module[
    {nbSpec, nb, data, results = <||>, allCells},
    nbSpec = Lookup[params, "notebook", "InputNotebook"];
    nb = getNotebook[nbSpec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>] ];

    data = params;
    allCells = Cells[nb];

    (* Helper to update by CellTag *)
    UpdateByTag[tag_, content_] := Module[{cells, target},
        cells = Cells[nb, CellTags -> tag];
        If[Length[cells] > 0,
            target = First[cells];
            SelectionMove[target, All, CellContents];
            NotebookWrite[nb, content];
            True,
            False
        ]
    ];

    (* Helper to find and update first Input cell after a tagged Section *)
    UpdateInputAfterTag[tag_, content_] := Module[{sectionCells, sectionIdx, nextCells, inputCell},
        sectionCells = Cells[nb, CellTags -> tag];
        If[Length[sectionCells] == 0, Return[False]];
        sectionIdx = FirstPosition[allCells, First[sectionCells], {0}][[1]];
        If[sectionIdx == 0, Return[False]];
        nextCells = Drop[allCells, sectionIdx];
        inputCell = SelectFirst[nextCells,
            MemberQ[Flatten[{CurrentValue[#, CellStyle]}], "Input"] &, $Failed];
        If[FailureQ[inputCell], Return[False]];
        SelectionMove[inputCell, All, CellContents];
        NotebookWrite[nb, content];
        True
    ];

    If[KeyExistsQ[data, "Title"],
        results["Title"] = UpdateByTag["Name", data["Title"]]
    ];
    If[KeyExistsQ[data, "Description"],
        results["Description"] = UpdateByTag["Description", data["Description"]]
    ];
    If[KeyExistsQ[data, "Definition"],
        results["Definition"] = UpdateInputAfterTag["Definition", data["Definition"]]
    ];
    If[KeyExistsQ[data, "Usage"],
        results["Usage"] = UpdateByTag["Usage", data["Usage"]]
    ];

    <|"success" -> True, "updates" -> results|>
];

(* Get cell info by index - for navigating and inspecting notebook structure *)
ExecuteNotebookCommand["GetCellByIndex", params_Association] := Module[
    {spec, idx, nb, cells, cell, style, tags, content},
    spec = Lookup[params, "notebook", "InputNotebook"];
    idx = Lookup[params, "index", 1];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    cells = Cells[nb];
    If[idx < 1 || idx > Length[cells], Return[<|"error" -> "Index out of range", "cellCount" -> Length[cells]|>]];

    cell = cells[[idx]];
    style = CurrentValue[cell, CellStyle];
    tags = CurrentValue[cell, CellTags];
    content = NotebookRead[cell];

    <|"success" -> True, "index" -> idx, "style" -> ToString[style], "tags" -> tags,
      "contentPreview" -> StringTake[ToString[content], UpTo[200]]|>
];

(* Silent evaluation - evaluate code in notebook session without leaving cells *)
ExecuteNotebookCommand["EvaluateSilent", params_Association] := Module[
    {spec, code, nb, result},
    spec = Lookup[params, "notebook", "InputNotebook"];
    code = Lookup[params, "code", "Null"];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    (* Use MathLink to evaluate in notebook's kernel without creating cells *)
    result = Quiet@Check[
        With[{expr = ToExpression[code, StandardForm, Hold]},
            ReleaseHold[expr]
        ],
        $Failed
    ];

    <|"success" -> True, "result" -> ToString[result, InputForm]|>
];

(* Trigger a button programmatically by extracting and executing its ButtonFunction *)
ExecuteNotebookCommand["TriggerButton", params_Association] := Module[
    {spec, nb, label, index, cells, cellExprs, buttonBoxes, dockedButtons,
     allButtons, targetButton, buttonFunc, result, includeDockedCells},
    spec = Lookup[params, "notebook", "InputNotebook"];
    label = Lookup[params, "label", Missing[]];
    index = Lookup[params, "index", 1];
    includeDockedCells = Lookup[params, "includeDockedCells", False];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    (* Read all cells and extract ButtonBoxes *)
    cells = Cells[nb];
    cellExprs = NotebookRead /@ cells;
    buttonBoxes = Flatten[Cases[#, ButtonBox[lbl_, opts___] :> {lbl, {opts}}, Infinity] & /@ cellExprs, 1];

    (* Optionally include buttons from DockedCells *)
    If[includeDockedCells,
        dockedButtons = Cases[CurrentValue[nb, DockedCells],
            ButtonBox[lbl_, opts___] :> {lbl, {opts}}, Infinity];
        allButtons = Join[buttonBoxes, dockedButtons],
        allButtons = buttonBoxes
    ];

    If[Length[allButtons] == 0,
        Return[<|"error" -> "No buttons found", "includeDockedCells" -> includeDockedCells|>]
    ];

    (* Find target button by label or index *)
    If[!MissingQ[label],
        targetButton = SelectFirst[allButtons,
            StringContainsQ[ToString[#[[1]]], label, IgnoreCase -> True] &, Missing[]];
        If[MissingQ[targetButton],
            Return[<|"error" -> "Button with label not found", "label" -> label,
                     "availableLabels" -> (ToString[#[[1]]] & /@ Take[allButtons, UpTo[10]])|>]
        ],
        If[index > Length[allButtons],
            Return[<|"error" -> "Button index out of range", "buttonCount" -> Length[allButtons]|>]
        ];
        targetButton = allButtons[[index]]
    ];

    If[MissingQ[targetButton], Return[<|"error" -> "Button not found"|>]];

    (* Extract ButtonFunction from options *)
    buttonFunc = ButtonFunction /. targetButton[[2]] /. ButtonFunction -> (Null &);

    (* Execute the button function *)
    result = Quiet@Check[buttonFunc[], $Failed];

    <|"success" -> True, "buttonLabel" -> ToString[targetButton[[1]]],
      "result" -> ToString[result, InputForm]|>
];

(* Get stylesheet definitions for exploring docked cell buttons *)
ExecuteNotebookCommand["GetStyleDefinitions", params_Association] := Module[
    {spec, nb, styleDefs, dockedCells, buttonCount},
    spec = Lookup[params, "notebook", "InputNotebook"];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    styleDefs = CurrentValue[nb, StyleDefinitions];
    dockedCells = CurrentValue[nb, DockedCells];
    buttonCount = Length[Cases[dockedCells, ButtonBox[__], Infinity]];

    <|"success" -> True,
      "styleDefinitions" -> ToString[styleDefs],
      "dockedCellsPreview" -> StringTake[ToString[dockedCells], UpTo[500]],
      "dockedCellButtonCount" -> buttonCount|>
];

(* Set a simple docked cell with button on a notebook - uses Button like a user would *)
ExecuteNotebookCommand["SetDockedCell", params_Association] := Module[
    {spec, nb, buttonLabel, buttonAction, buttonExpr},
    spec = Lookup[params, "notebook", "InputNotebook"];
    buttonLabel = Lookup[params, "label", "Test Button"];
    buttonAction = Lookup[params, "action", "Print[\"Button clicked!\"]"];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    (* Create docked cell with simple Button expression - like user would in notebook *)
    buttonExpr = ToExpression["Button[\"" <> buttonLabel <> "\", " <> buttonAction <> "]"];
    SetOptions[nb, DockedCells -> {
        Cell[BoxData[ToBoxes[buttonExpr]], "DockedCell", Background -> LightBlue]
    }];

    <|"success" -> True, "label" -> buttonLabel, "action" -> buttonAction|>
];

(* List all buttons in docked cells of a notebook *)
ExecuteNotebookCommand["ListDockedButtons", params_Association] := Module[
    {spec, nb, dockedCells, buttonBoxes, buttonLabels},
    spec = Lookup[params, "notebook", "InputNotebook"];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    dockedCells = CurrentValue[nb, DockedCells];
    buttonBoxes = Cases[dockedCells, ButtonBox[lbl_, opts___] :> {lbl, {opts}}, Infinity];
    buttonLabels = StringTake[ToString[#[[1]]], UpTo[50]] & /@ buttonBoxes;

    <|"success" -> True, "buttonCount" -> Length[buttonBoxes],
      "buttonLabels" -> buttonLabels|>
];

(* Select and trigger a docked cell button using Cells + DockedCell approach *)
ExecuteNotebookCommand["TriggerDockedButtonByCell", params_Association] := Module[
    {spec, nb, dockedCells, cellContent, buttonBoxes, buttonIndex, targetButton, buttonFunc, result},
    spec = Lookup[params, "notebook", "InputNotebook"];
    buttonIndex = Lookup[params, "index", 1];
    nb = getNotebook[spec];
    If[!MatchQ[nb, _NotebookObject], Return[<|"error" -> "Notebook not found"|>]];

    (* Get docked cells using DockedCell option *)
    dockedCells = Cells[nb, DockedCell -> True];
    If[Length[dockedCells] == 0,
        Return[<|"error" -> "No docked cells found", "note" -> "Try ListDockedButtons instead"|>]
    ];

    (* Read all docked cells and extract buttons *)
    cellContent = NotebookRead /@ dockedCells;
    buttonBoxes = Flatten[Cases[#, ButtonBox[lbl_, opts___] :> {lbl, {opts}}, Infinity] & /@ cellContent, 1];

    If[Length[buttonBoxes] == 0,
        Return[<|"error" -> "No buttons found in docked cells", "dockedCellCount" -> Length[dockedCells]|>]
    ];

    If[buttonIndex > Length[buttonBoxes],
        Return[<|"error" -> "Button index out of range", "buttonCount" -> Length[buttonBoxes]|>]
    ];

    targetButton = buttonBoxes[[buttonIndex]];

    (* Extract and execute ButtonFunction *)
    buttonFunc = ButtonFunction /. targetButton[[2]] /. ButtonFunction -> (Null &);
    result = Quiet@Check[buttonFunc[], $Failed];

    <|"success" -> True, "buttonLabel" -> StringTake[ToString[targetButton[[1]]], UpTo[50]],
      "dockedCellCount" -> Length[dockedCells], "buttonCount" -> Length[buttonBoxes],
      "result" -> ToString[result, InputForm]|>
];

(* Test SimulateMouseClick on a position in notebook *)
ExecuteNotebookCommand["ClickAtPosition", params_Association] := Module[
    {x, y},
    x = Lookup[params, "x", 100];
    y = Lookup[params, "y", 100];

    FrontEndExecute[FrontEnd`SimulateMouseClick[$FrontEndSession, {x, y}]];

    <|"success" -> True, "clickedAt" -> {x, y}|>
];

(* ::Subsection::Closed:: *)
(*Button and Mouse Commands*)

(* Helper to bring Wolfram Desktop to front and activate it *)
focusWolframDesktop[] := Module[{result},
    (* Use System Events to set frontmost - works with WolframNB process name *)
    result = RunProcess[{
        "osascript", "-e",
        "tell application \"System Events\" to set frontmost of process \"WolframNB\" to true"
    }];
    Pause[0.15]; (* Small pause for activation *)
    result["ExitCode"] === 0
];

(* Helper to bring the WolframNotebook palette back to front *)
bringPaletteToFront[] := Module[{nbs, palette},
    nbs = Notebooks[];
    palette = SelectFirst[nbs, StringContainsQ[ToString[#], "WolframNotebook"] &];
    If[MatchQ[palette, _NotebookObject],
        SetSelectedNotebook[palette];
        FrontEndExecute[FrontEnd`NotebookSuspendScreenUpdates[palette]]; (* Minimal visual disruption *)
    ]
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

    (* Adjust position - GetBoxPositions returns positions offset ~36px left of center *)
    pos = pos + {36, 0};

    (* First select and bring the TARGET notebook to front within Mathematica *)
    SetSelectedNotebook[nb];
    FrontEndExecute[FrontEnd`NotebookBringToFront[nb]];

    (* Then bring Wolfram Desktop to front *)
    focusWolframDesktop[];
    Pause[0.25]; (* Delay for window to come to front *)

    (* Move to position first *)
    moveMouse[All, pos, "Delay" -> 0.05];
    Pause[0.1];

    (* Use FrontEnd`SimulateMouseClick directly *)
    FrontEndExecute[FrontEnd`SimulateMouseClick[nb, pos, "Left", 1]];

    <|
        "success" -> True,
        "clicked" -> pos,
        "buttonIndex" -> index,
        "totalButtons" -> If[ListQ[positions], Length[positions], 0],
        "allButtonPositions" -> If[ListQ[positions], positions, {}],
        "notebook" -> ToString[nb],
        "target" -> If[StringQ[target], target, "none"]
    |>
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

    (* Click at coordinates with instant movement *)
    moveMouse[If[scope === "All", All, InputNotebook[]], {x, y}, "Click", "Delay" -> 0];

    <|"success" -> True, "clicked" -> {x, y}, "scope" -> scope|>
];

ExecuteNotebookCommand["SendKeys", params_Association] := Module[
    {text, spec, nb, moveMouse},
    text = Lookup[params, "text", ""];
    spec = Lookup[params, "notebook", "InputNotebook"];

    If[!StringQ[text] || text === "",
        Return[<|"error" -> "text parameter required"|>]
    ];

    nb = getNotebook[spec];
    moveMouse = ResourceFunction["MoveMouse"];

    (* Focus the window and send keystrokes *)
    focusWolframDesktop[];
    If[MatchQ[nb, _NotebookObject], SetSelectedNotebook[nb]];
    Pause[0.1];
    moveMouse[If[MatchQ[nb, _NotebookObject], nb, All], {0, 0}, {"Type", text}, "Delay" -> 0];

    <|"success" -> True, "typed" -> text|>
];

(* Send arrow key events for games and interactive elements *)
ExecuteNotebookCommand["SendArrowKey", params_Association] := Module[
    {direction, spec, nb, keyName, timestamp},
    direction = ToLowerCase[Lookup[params, "direction", ""]];
    spec = Lookup[params, "notebook", "InputNotebook"];
    timestamp = DateString[{"Hour", ":", "Minute", ":", "Second", ".", "Millisecond"}];

    keyName = Switch[direction,
        "up" | "w", "UpArrowKeyDown",
        "down" | "s", "DownArrowKeyDown",
        "left" | "a", "LeftArrowKeyDown",
        "right" | "d", "RightArrowKeyDown",
        _, None
    ];

    If[keyName === None,
        Return[<|"error" -> "Invalid direction. Use: up, down, left, right (or w, a, s, d)"|>]
    ];

    nb = getNotebook[spec];
    focusWolframDesktop[];
    If[MatchQ[nb, _NotebookObject], SetSelectedNotebook[nb]];
    Pause[0.1];

    (* Use FrontEnd`SimulateKeyPress to send keyboard events *)
    FrontEndExecute[FrontEnd`SimulateKeyPress[nb, keyName]];

    <|
        "success" -> True,
        "direction" -> direction,
        "keyName" -> keyName,
        "notebook" -> ToString[nb],
        "spec" -> spec,
        "timestamp" -> timestamp
    |>
];

(* Default handler for unknown commands *)
ExecuteNotebookCommand[cmd_, _] := <|"error" -> "Unknown command: " <> ToString[cmd]|>;

End[];
EndPackage[];
