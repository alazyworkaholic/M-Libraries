//Copy this code into a new query and use the result to load a more complex set of expressions from a file while being tolerant of faults.
let
    LoadOptionsType =
        type [
            optional Errors = (Logical.Type
            meta
            [
                Documentation.FieldDescription = "Return errors when evaluating a code section. Defaults to <code>false</code>."
            ]),
            optional Shared = (Logical.Type
            meta
            [
                Documentation.FieldDescription = "Return only shared statements. Defaults to <code>true</code>."
            ])
        ],
    _Options =
        (Options) =>
            [
                Shared = true,
                Errors = false
            ]
            & (Options ?? []),
    Code.SectionToExpressions =
        let
            FunctionDocumentation = [
                Documentation.Name = "Code.SectionToExpressions",
                Documentation.Description = "Crudely transforms a single M section into a list of expressions by splitting where semicolons are followed by line breaks.",
                Documentation.Examples = {
                    [
                        Description = "Transform a section into a list of expressions.",
                        Code = "Code.SectionToExpressions(""section MySection;#(cr,lf)My.Function = () => 0;#(cr,lf)My.Constant = 1;",
                        Result = "{""My.Function = () => 0"", ""My.Constant = 1""}"
                    ]
                }
            ],
            FunctionType = type function (Text as text) as {text},
            FunctionImplementation =
                (Text as text) as list =>
                    let
                        StatementSplitter =
                            Splitter.SplitTextByAnyDelimiter(
                                {
                                    ";#(cr,lf)",
                                    ";#(cr)",
                                    ";#(lf)"
                                },
                                QuoteStyle.None
                            //Very important to avoid losing "
                            ),
                        Statements = List.Skip(StatementSplitter(Text)),
                        RemovedTrailingSemicolon =
                            List.FirstN(
                                Statements,
                                List.Count(Statements) - 1
                            )
                            & {
                                Text.TrimEnd(List.Last(Statements), ";")
                            }
                    in
                        RemovedTrailingSemicolon
        in
            Value.ReplaceType(
                FunctionImplementation,
                FunctionType meta FunctionDocumentation
            ),
    Expression.EvaluateMany =
        let
            FunctionDocumentation = [
                Documentation.Name = "Expression.EvaluateMany",
                Documentation.Description = "Tolerantly evaluates a list of M expressions that may depend on each other.",
                Documentation.Examples = {
                    [
                        Description = "Evaluate a list of M expressions.",
                        Code = "Expression.EvaluateMany({""Y = () => My.Function(0)"",""My.Function = (X) => X"", ""Z = Y()""})",
                        Result = "[Y = () => My.Function(0), My.Function = (X) => X, Z = 0]"
                    ]
                }
            ],
            FunctionType = type function (Expressions as {text}, optional Options as LoadOptionsType) as anynonnull,
            FunctionImplementation =
                (Expressions as list, optional Options as record) as anynonnull =>
                    let
                        Start =
                            () =>
                                [
                                    Code =
                                        #table(
                                            type table [Code = text],
                                            List.Transform(Expressions, each
                                                {
                                                    _
                                                })
                                        ),
                                    Evaluated = [],
                                    Previous = -1
                                ],
                        Condition =
                            each
                                Record.FieldCount([Evaluated])
                                > [Previous],
                        Next =
                            (Record as record) =>
                                let
                                    Code = Record[Code],
                                    Result = Record[Evaluated],
                                    EvaluateScalarVector =
                                        Function.ScalarVector(
                                            type function (Code as text) as text,
                                            (InputTable) =>
                                                List.Transform(
                                                    Table.Buffer(InputTable)[Code],
                                                    each
                                                        Expression.Evaluate(
                                                            "[" & _ & "]",
                                                            #shared & Result
                                                        )
                                                )
                                        ),
                                    Evaluated =
                                        Table.AddColumn(
                                            Table.SelectColumns(Code, "Code"),
                                            "Evaluated",
                                            each EvaluateScalarVector([Code]),
                                            type record
                                        ),
                                    Selector = (RowSelector) => RowSelector(Evaluated, {"Evaluated"}),
                                    Successes = Selector(Table.RemoveRowsWithErrors),
                                    Errors = Selector(Table.SelectRowsWithErrors),
                                    NewResult =
                                        Result
                                        & Record.Combine(
                                            List.TransformMany(
                                                Successes[Evaluated],
                                                Record.FieldNames,
                                                (Record, Name) =>
                                                    if Text.StartsWith(Name, "shared ") then
                                                        Record.RenameFields(
                                                            Record,
                                                            {
                                                                {
                                                                    Name,
                                                                    Text.AfterDelimiter(Name, "shared ")
                                                                }
                                                            }
                                                        )
                                                    else
                                                        Record
                                            )
                                        ),
                                    Return = [
                                        Code = Errors,
                                        Evaluated = NewResult,
                                        Previous = Record.FieldCount(Result)
                                    ]
                                in
                                    Return,
                        Result =
                            List.Last(
                                List.Generate(
                                    Start,
                                    Condition,
                                    Next,
                                    each
                                        if _Options(Options)[Errors] then
                                            [Code]
                                        else
                                            Record.SelectFields(
                                                [Evaluated],
                                                List.Sort(Record.FieldNames([Evaluated]))
                                            )
                                )
                            )
                    in
                        Result
        in
            Value.ReplaceType(
                FunctionImplementation,
                FunctionType meta FunctionDocumentation
            ),
    Code.Load =
        let
            FunctionDocumentation = [
                Documentation.Name = "Code.Load",
                Documentation.Descriptoin = "Evaluates M code contained in <code>Source</code> and returns it in a record, with an option to inspect evaluation errors.",
                Documentation.Examples = {
                    [
                        Description = "Load a file containing <code>""section MyCode;#(cr,lf)My.Function = () => 0;#(cr,lf)My.Constant = 1""</code>",
                        Code = "Code.Load(""C:\MyCode.pq"")",
                        Result = "[My.Function = () => 0, My.Constant = 1]"
                    ]
                }
            ],
            FunctionType = type function (Source as text, Type as
                (
                    Text.Type
                    meta
                    [
                        Documentation.AllowedValues = {
                            "Expression",
                            "Section"
                        },
                        Documentation.DefaultValue = "Section",
                        Documentation.FieldDescription = "Indicate whether the Code is a single M expression, or an M section document containing various expressions separated by semicolons."
                    ]
                ), optional Options as LoadOptionsType) as anynonnull,
            FunctionImplementation =
                (Source as text, Type as text, optional Options as record) as anynonnull =>
                    List.Accumulate(
                        {
                            Web.Contents,
                            Lines.FromBinary,
                            each List.Transform(_, Text.Trim),
                            Lines.ToBinary,
                            Text.FromBinary,
                            each
                                if Type = "Section" then
                                    Expression.EvaluateMany(
                                        Code.SectionToExpressions(_),
                                        Options
                                    )
                                else
                                    Expression.Evaluate(_)
                        },
                        Source,
                        (State, Current) => Current(State)
                    )
        in
            Value.ReplaceType(
                FunctionImplementation,
                FunctionType meta FunctionDocumentation
            )
in
    Code.Load
