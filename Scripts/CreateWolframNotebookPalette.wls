#!/usr/bin/env wolframscript
(* ::Package:: *)
(* WolframNotebook Palette Generator *)
(* Run this script to create and optionally install the palette *)

BeginPackage["Wolfram`MCPServer`WolframNotebookPalette`"];

CreateWolframNotebookPalette::usage = "CreateWolframNotebookPalette[] creates and displays the WolframNotebook palette.";
InstallWolframNotebookPalette::usage = "InstallWolframNotebookPalette[] installs the palette to the user's Palettes directory.";
ExportWolframNotebookPalette::usage = "ExportWolframNotebookPalette[] exports the palatte."

Begin["`Private`"];

$defaultPort = 50000;

(* Socket event handler *)
handleSocketEvent[Dynamic[status_], Dynamic[clients_]][event_Association] := Module[
    {data, request, command, params, result, response, sourceSocket},

    sourceSocket = event["SourceSocket"];

    (* Track connected clients *)
    If[!MemberQ[clients, sourceSocket],
        AppendTo[clients, sourceSocket];
        status = "Connected"
    ];

    (* Parse the incoming data *)
    data = event["Data"];
    If[!StringQ[data] || data === "", Return[]];

    Quiet[
        request = ImportString[StringTrim[data], "RawJSON"];
        If[!AssociationQ[request], Return[]];

        command = Lookup[request, "command", "unknown"];
        params = Lookup[request, "params", <||>];

        (* Execute the command *)
        result = executeCommand[command, params];

        (* Send response *)
        response = ExportString[<|
            "id" -> Lookup[request, "id", "unknown"],
            "status" -> If[KeyExistsQ[result, "error"], "error", "success"],
            "result" -> result,
            If[KeyExistsQ[result, "error"], "error" -> result["error"], Nothing]
        |>, "JSON"];

        WriteString[sourceSocket, response <> "\n"]
    ]
];

(* Command handlers *)
executeCommand["Ping", _] := <|"pong" -> True, "timestamp" -> DateString[]|>;

executeCommand["GetNotebookImage", params_Association] := Module[
    {notebookSpec, resolution, nb, img, base64, dims},

    notebookSpec = Lookup[params, "notebook", "InputNotebook"];
    resolution = Lookup[params, "resolution", 144];

    nb = Switch[notebookSpec,
        "InputNotebook", InputNotebook[],
        "EvaluationNotebook", EvaluationNotebook[],
        "SelectedNotebook", SelectedNotebook[],
        _,
        With[{nbs = Select[Notebooks[],
            StringContainsQ[ToString[#], notebookSpec, IgnoreCase -> True] &]},
            If[Length[nbs] > 0, First[nbs], InputNotebook[]]
        ]
    ];

    If[!MatchQ[nb, _NotebookObject],
        Return[<|"error" -> "Could not find notebook"|>]
    ];

    img = Rasterize[nb, ImageResolution -> resolution];
    If[!ImageQ[img],
        Return[<|"error" -> "Failed to rasterize notebook"|>]
    ];

    dims = ImageDimensions[img];
    base64 = BaseEncode[ExportByteArray[img, "PNG"]];

    <|"image" -> base64, "width" -> dims[[1]], "height" -> dims[[2]]|>
];

executeCommand["ListNotebooks", _] := <|
    "notebooks" -> Map[
        Function[nb, <|
            "id" -> ToString[nb],
            "title" -> Quiet[CurrentValue[nb, WindowTitle]]
        |>],
        Notebooks[]
    ]
|>;

executeCommand[cmd_, _] := <|"error" -> "Unknown command: " <> ToString[cmd]|>;

(* Create the palette *)
CreateWolframNotebookPalette[] := CreatePalette[
    DynamicModule[{status = "Disconnected", listener = None, port = $defaultPort, clients = {}},
        Framed[
            Panel[
                Column[{
                    Style["WolframNotebook Server", Bold, 14, GrayLevel[0.3]],

                    (* Status indicator *)
                    Dynamic @ Row[{
                        Graphics[{
                            Switch[status,
                                "Connected", Darker@Green,
                                "Listening", Darker@Orange,
                                _, Darker@Red
                            ],
                            Disk[{0, 0}, 0.4]
                        }, ImageSize -> 12],
                        " ",
                        Style[status,
                            Switch[status, "Connected", Darker@Green, "Listening", Darker@Orange, _, Darker@Red],
                            Bold
                        ]
                    }],

                    (* Client count *)
                    Dynamic @ If[Length[clients] > 0,
                        Style["Clients: " <> ToString[Length[clients]], Gray, 10],
                        ""
                    ],

                    (* Port input *)
                    Row[{"Port: ", InputField[Dynamic[port], Number, FieldSize -> 6,
                        Enabled -> Dynamic[listener === None]]}],

                    (* Start/Stop button *)
                    Dynamic @ Button[
                        If[listener === None, "Start Listening", "Stop"],
                        If[listener === None,
                            (* Start listening *)
                            listener = SocketListen[port,
                                handleSocketEvent[Dynamic[status], Dynamic[clients]]
                            ];
                            If[MatchQ[listener, _SocketListener],
                                status = "Listening",
                                status = "Error"; listener = None
                            ],
                            (* Stop listening *)
                            Quiet[
                                If[MatchQ[listener, _SocketListener],
                                    With[{sock = listener["Socket"]},
                                        DeleteObject[listener];
                                        Close[sock]
                                    ]
                                ]
                            ];
                            listener = None;
                            clients = {};
                            status = "Disconnected"
                        ],
                        Method -> "Queued",
                        ImageSize -> {120, 30}
                    ],

                    Style["Start listening before using\nWolframNotebook MCP tools", 9, Gray]
                }, Alignment -> Center, Spacings -> 0.5],
                Background -> GrayLevel[0.97]
            ],
            FrameStyle -> GrayLevel[0.8],
            RoundingRadius -> 5
        ]
    ],
    WindowTitle -> "WolframNotebook",
    WindowSize -> {180, Fit},
    WindowElements -> {},
    WindowFrameElements -> {"CloseBox"}
];

(* Export the palette notebook (works without frontend) *)
ExportWolframNotebookPalette[] := Module[{notebook, file, dir},
    notebook = Notebook[{
        Cell[BoxData[ToBoxes[
            DynamicModule[{status = "Disconnected", listener = None, port = $defaultPort, clients = {}, errorMsg = "", commandLog = {}},
                Column[{
                    Style["WolframNotebook Server", Bold, 11],
                    Dynamic @ Row[{
                        Graphics[{
                            Switch[status, "Connected", Darker@Green, "Listening", Darker@Orange, _, Darker@Red],
                            Disk[{0, 0}, 0.4]
                        }, ImageSize -> 10],
                        " ",
                        Style[status, Switch[status, "Connected", Darker@Green, "Listening", Darker@Orange, _, Darker@Red], Bold, 10]
                    }],
                    Dynamic[If[errorMsg =!= "", Tooltip[Style[StringTake[errorMsg, UpTo[20]], Red, 7], errorMsg], Spacer[1]], TrackedSymbols :> {errorMsg}],
                    Dynamic[If[Length[clients] > 0, Style["Clients: " <> ToString[Length[clients]], 8], Spacer[1]], TrackedSymbols :> {clients}],
                    Row[{Style["Port:", 9], Spacer[2], InputField[Dynamic[port], Number, FieldSize -> {{4, 4}, {1, 1}}, Enabled -> Dynamic[listener === None]]}],
                    Dynamic @ Button[
                        If[listener === None, "Start", "Stop"],
                        If[listener === None,
                            errorMsg = "";
                            (* Close any existing listeners on this port *)
                            Quiet[Scan[Close, Select[Sockets[], #["DestinationPort"] === port &]]];
                            Module[{result, msg},
                                result = SocketListen[port, Function[event, handleSocketEvent[Dynamic[status], Dynamic[clients], Dynamic[commandLog]][event]]];
                                If[MatchQ[result, _SocketListener],
                                    listener = result; status = "Listening",
                                    status = "Error"; listener = None;
                                    msg = If[MatchQ[result, _Failure], result["Message"], "Port unavailable"];
                                    errorMsg = ToString[msg];
                                    Message[SocketListen::err, port, msg]
                                ]
                            ],
                            Quiet[If[MatchQ[listener, _SocketListener], With[{sock = listener["Socket"]}, DeleteObject[listener]; Close[sock]]]];
                            listener = None; clients = {}; status = "Disconnected"; errorMsg = ""
                        ],
                        Method -> "Queued",
                        ImageSize -> {60, 20}
                    ],
                    (* Command log display - always show section with scrollbar *)
                    Style["Recent Commands:", 8, Bold],
                    Pane[
                        Dynamic[
                            If[Length[commandLog] > 0,
                                Column[
                                    Style[#, 7, Gray] & /@ Take[commandLog, UpTo[10]],
                                    Spacings -> 0
                                ],
                                Style["Waiting for commands...", 7, LightGray]
                            ],
                            TrackedSymbols :> {commandLog}
                        ],
                        ImageSize -> {180, 80},
                        Scrollbars -> {False, Automatic},
                        AppearanceElements -> None
                    ]
                }, Alignment -> Center, Spacings -> 0.1],
                Initialization :> (
                    SocketListen::err = "Port `1`: `2`";

                    (* Load the paclet once at initialization *)
                    PacletDirectoryLoad["/Users/swish/src/wolfram/MCPServer"];
                    Get["Wolfram`MCPServer`NotebookCommands`"];

                    handleSocketEvent[Dynamic[statusVar_], Dynamic[clientsVar_], Dynamic[logVar_]][event_Association] := Module[
                        {data, request, command, params, result, response, sourceSocket, timestamp},
                        sourceSocket = event["SourceSocket"];
                        If[!MemberQ[clientsVar, sourceSocket], AppendTo[clientsVar, sourceSocket]; statusVar = "Connected"];
                        data = event["Data"];
                        If[!StringQ[data] || data === "", Return[]];
                        Quiet[
                            request = ImportString[StringTrim[data], "RawJSON"];
                            If[!AssociationQ[request], Return[]];
                            command = Lookup[request, "command", "unknown"];
                            params = Lookup[request, "params", <||>];

                            (* Log the command with key params *)
                            timestamp = DateString[{"Hour", ":", "Minute", ":", "Second"}];
                            Module[{logEntry, paramStr},
                                paramStr = Switch[command,
                                    "ClickButton", " idx:" <> ToString[Lookup[params, "index", "?"]] <> " nb:" <> StringTake[ToString[Lookup[params, "notebook", "?"]], UpTo[10]],
                                    "SendArrowKey", " " <> ToString[Lookup[params, "direction", "?"]],
                                    "MouseClick", " x:" <> ToString[Lookup[params, "x", "?"]] <> " y:" <> ToString[Lookup[params, "y", "?"]],
                                    _, ""
                                ];
                                logEntry = timestamp <> " " <> command <> paramStr;
                                PrependTo[logVar, logEntry]
                            ];
                            If[Length[logVar] > 20, logVar = Take[logVar, 20]];

                            (* Pure dispatcher - all commands go to paclet *)
                            result = Wolfram`MCPServer`NotebookCommands`ExecuteNotebookCommand[command, params];

                            response = ExportString[<|
                                "id" -> Lookup[request, "id", "unknown"],
                                "status" -> If[KeyExistsQ[result, "error"], "error", "success"],
                                "result" -> result,
                                If[KeyExistsQ[result, "error"], "error" -> result["error"], Nothing]
                            |>, "JSON"];
                            WriteString[sourceSocket, response <> "\n"]
                        ]
                    ];

                    (* Auto-start listener *)
                    If[listener === None,
                        commandLog = {"Palette initialized"};
                        Quiet[Scan[Close, Select[Sockets[], #["DestinationPort"] === port &]]];
                        With[{res = SocketListen[port, Function[event, handleSocketEvent[Dynamic[status], Dynamic[clients], Dynamic[commandLog]][event]]]},
                             If[MatchQ[res, _SocketListener],
                                 listener = res; status = "Listening";
                                 PrependTo[commandLog, "Listening on port " <> ToString[port]]
                             ]
                        ]
                    ];
                )
            ]
        ]], "Output",
            CellMargins -> {{5, 5}, {5, 5}},
            Editable -> True,
            Selectable -> True
        ]
    },
        WindowTitle -> "WolframNotebook",
        WindowSize -> {Fit, Fit},
        WindowElements -> {},
        WindowFrameElements -> {"CloseBox"},
        Saveable -> False,
        WindowFloating -> True,
        WindowClickSelect -> False, (* Don't steal focus when clicking *)
        Editable -> True,
        Selectable -> True,
        ShowCellBracket -> False,
        WindowToolbars -> {},
        StyleDefinitions -> "Palette.nb"
    ];

    dir = FileNameJoin[{DirectoryName[$InputFileName, 2], "FrontEnd", "Palettes"}];
    If[!DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
    file = FileNameJoin[{dir, "WolframNotebook.nb"}];
    Export[file, notebook, "NB"];
    Print["Palette exported to: ", file];
    file
];

(* Install the palette - use CreatePalette if frontend available, otherwise export directly *)
InstallWolframNotebookPalette[] := If[$Notebooks === True,
    Module[{nb, file},
        nb = CreateWolframNotebookPalette[];
        file = FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "Palettes", "WolframNotebook.nb"}];
        NotebookSave[nb, file];
        NotebookClose[nb];
        Print["Palette installed to: ", file];
        file
    ],
    ExportWolframNotebookPalette[]
];

End[];
EndPackage[];

(* If run as a script, export the palette *)
If[$ScriptCommandLine =!= {},
    Needs["Wolfram`MCPServer`WolframNotebookPalette`"];
    ExportWolframNotebookPalette[]
];
