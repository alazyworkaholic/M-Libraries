section Fns;
//Original Author: Alan Thomas
/* Troubleshooting:
A .pbix that references one of these functions won't load (XMLA endpoint error) and a blank .pbix can't find any of these functions: At least one of the functions cannot be evaluated correctly. In one case, Function.Document was taking (Name, Description, null, Examples, Implementation) instead of (Name, Description, Examples, Type, Implementation).
*/
/* TODO:
Unit Tests, preferably leveraging the Documentation.Examples of each function
*/

shared Date.Calendar = Function.Document(
	"Date.Calendar",
	"Creates a calendar table spanning the specified dates.",
	null,
	type function (
		#"Start Date" as date,
		#"End Date" as date,
		optional Columns as { [ Name = text, Function = function ] }
	) as table,
	let
	DefaultCalendarColumns = {
		[ Name = "Date", Function = (Date as date) as date => Date ],
		[ Name = "Year", Function = Date.Year ],
		[ Name = "Month", Function = (Date as date) as text => Date.ToText(Date, "MMMM") ],
		[ Name = "Mth", Function = (Date as date) as text => Date.ToText(Date, "MMM") ],
		[ Name = "Month of Year", Function = Date.Month ],
		[ Name = "YYYY MM", Function = (Date as date) as text => Date.ToText(Date, "yyyy MM") ],
		[ Name = "YYYYMM", Function = Value.ReplaceType( (Date as date) as number => Number.From(Date.Year(Date)) * 100 + Number.From(Date.Month(Date)), type function (Date as date) as Int32.Type ) ],
		[ Name = "Week of Year", Function = Date.WeekOfYear ],
		[ Name = "Week of Month", Function = Date.WeekOfMonth ],
		[ Name = "Day of Week", Function = Date.DayOfWeek ],
		[ Name = "Day of Month", Function = Date.Day ],
		[ Name = "Day of Year", Function = Date.DayOfYear ],
		[ Name = "Day Name", Function = Date.DayOfWeekName ],
		[ Name = "Is Weekend", Function = (Date as date) as logical => ( Date.DayOfWeek(Date, Day.Sunday) = Day.Saturday ) or ( Date.DayOfWeek(Date, Day.Sunday) = Day.Sunday ) ]
	},
	Implementation = (Start as date, End as date, optional Columns as nullable list) as table => let
		CalendarColumns = if Columns is null then
				DefaultCalendarColumns
			else
				Columns,
		Dates = List.Dates(
			Start,
			Duration.Days(End - Start) + 1,
			#duration(1, 0, 0, 0)
		),
		Names = List.Transform(
			CalendarColumns,
			each [Name]
		),
		Types = List.Transform(
			CalendarColumns,
			each Type.FunctionReturn(Value.Type([Function]))
		),
		TableType = Type.ForTable(Names, Types),
		ColumnFunctions = List.Transform(
			CalendarColumns,
			each [Function]
		),
		DateData = List.Transform(
			Dates,
			(Date as date) =>
				List.Transform(
					ColumnFunctions,
					each _(Date)
				)
		),
		Table = #table(TableType, DateData)
	in Table
in Implementation);

shared Date.IsContinuous = let
	Documentation = [
		Documentation.Name = "Date.IsContinuous",
		Documentation.Description = "Determines whether a list of dates is continuous.",
		Documentation.Examples = { [
			Description = "Informs that {#date(2020,1,1),#date(2020,1,2),#date(2020,1,3)} are continuous.",
			Code = "Date.IsContinuous({#date(2020,1,1),#date(2020,1,2),#date(2020,1,3)})",
			Result = "true"
		] }
	],
	Type = type function (Dates as {date}) as Logical.Type,
	Implementation = (Dates as list) as logical => List.IsEmpty(
		List.RemoveMatchingItems(
			Date.Span(Dates),
			Dates
	) )
in Value.ReplaceType( Implementation, Type meta Documentation );

shared Date.Span = Function.Document(
	"Date.Span",
	"Create an uninterrupted sequence of dates that spans the first date in the list to the last.",
	{ [
		Description = "Return consecutive dates from the first to the last day of a sparse date list.",
		Code = "Date.Span({#date(2020,1,1), #date(2020,1,3), #date(2020,1,5)})",
		Result = "{#date(2020,1,1), #date(2020,1,2), #date(2020,1,3), #date(2020,1,4), #date(2020,1,5)}"
	] },
	type function (Dates as {date}) as {date},
	(Dates as list) as list => let
		StartDate = List.Min(List.RemoveNulls(Dates)),
		EndDate = List.Max(Dates),
		Result = List.Dates(
			StartDate,
			Duration.Days(EndDate - StartDate) + 1,
			#duration(1, 0, 0, 0)
		)
	in Result
) ;

/*
	//Function.AlterArguments = ...,
	/* Useful for debugging?
Function.ExtractArguments = let
	Documentation = [
		Documentation.Name = ...,
		Documentation.Description = "Modify a function so that it returns the list of arguments passed to it instead of the result of invoking it.",
		Documentation.Examples = ...
	],
	Type = type function (Function as function) as list,
	Implementation = (Function as function) as list => let
		Type = Value.Type(Function),
		Signature = [
			ReturnType = type list,
			Parameters = Type.FunctionParameters(Type)
		],
		Required = Type.FunctionRequiredParameters(Type),
		NewFunctionType = Type.ForFunction( Signature, Required ),
		NewImplementation = (Arguments as list) as list => Arguments,
		Metadata = Value.Metadata(Type), //Annotate the documentation to disclose the new behaviour.
		NewDocString = "<b>NOTE:</b> Modified to return a value if the result of the function is null. Inspect function metadata to see the value.<p></p>",
		NewDocs =  [
			Documentation.Description = NewDocString & Metadata[Documentation.Description],
			Documentation.LongDescription = NewDocString & Metadata[Documentation.LongDescription]
		]
	in Function.From(NewFunctionType meta (Metadata & NewDocs), NewImplementation)
in Value.ReplaceType(Implementation, Type meta Documentation)
,
*/
shared Function.DelayInvocation = Function.Document(
//TODO Update metadata Documentation to note that the function has been modified.
	"Function.DelayInvocation",
	"Returns <code>Function</code> such that when called it will only be invoked after <code>Delay</code>.",
	{ [
		Description = "Delay current time by 5 seconds.",
		Code = "[Now = DateTime.LocalNow(), Delayed = Function.DelayInvocation(DateTime.LocalNow, #duration(0,0,0,5))()]",
		Result = "[Now = 2021-08-15 12:00:00 AM, Delayed = 2021-08-15 12:00:05 AM]"
	], [
		Description = "Certain functions run only once, even if they can be delayed.",
		Code = "[Now = DateTime.FixedLocalNow(), Delayed = Function.DelayInvocation(DateTime.FixedLocalNow, #duration(0,0,0,5))()]",
		Result = "[Now = 2021-08-15 12:00:05 AM, Delayed = 2021-08-15 12:00:05 AM]"
	] },
	type function (Function as function, Delay as duration) as function,
	(Function as function, Delay as duration) as function => let
		UnaryInvocation = (List as list) as any => Function.InvokeAfter(
			() => Function.Invoke(Function, List),
			Delay
		),
		DelayedFunction = Function.From(
			Value.Type(Function),
			UnaryInvocation
		)
	in DelayedFunction
);

//This function is critical to document functions elsewhere in the library.
shared Function.Document = let
	FunctionDocumentation = [
		Documentation.Name = "Function.Document",
		Documentation.Description = "Concisely ascribe <code>Type</code> and add documentation metadata to a function <code>Implementation</code>.",
		Documentation.Examples = {[
			Description = "Add documentation to a function.",
			Code = "Function.Function.Document(""Make.Zero"", ""Returns Zero"", {[Description = ""An Example"", Code = ""Make.Zero()"", Result = ""0""]}, type function () as number, () => 0  => 0",
			Result = "Value.ReplaceType(() => 0, type function () as number meta [Documentation.Name = ""Make.Zero"", Documentation.Description = ""Returns Zero"", Documentation.Examples = {[Description = ""An Example"", Code = ""Make.Zero()"", Result = ""0""]}])"
		]}
	],
	FunctionType = type function (Name as text, Description as nullable text, Examples as nullable list, Type as type, Implementation as function) as function,
	FunctionImplementation = (Name as text, Description as nullable text, Examples as nullable list, Type as type, Implementation as function) as function => Value.ReplaceType(
		Implementation,
		Type meta [
			Documentation.Name = Name, 
			Documentation.Description = if Description = null then "No Description." else Description,
			Documentation.Examples = if Examples = null then {} else Examples
		]
	)
in Value.ReplaceType(FunctionImplementation, FunctionType meta FunctionDocumentation);

shared Function.InterceptInvocation = Function.Document(
//TODO Update metadta Documentation to note that the function has been modified.
//TODO: Possible to include everything in function's evaluation scope as a record?
	"Function.InterceptInvocation",
	"Returns <code>Function</code> modified such that, when invoked, it returns a record consisting of <code>Function</code>, the <code>Parameters</code> that were used to invoke it, and the delayed <code>Result</code> of invoking <code>Function</code> with <code>Parameters</code>.",
	{ [
		Description = "Intercept invocation of a function.",
		Code = "Function.InterceptInvocation(Value.Equals)(1,1)",
		Result = "[Function = Value.Equals, Paramters = {1,1}, Result = true]"
	], [
		Description = "Inspect the parameters used to invoke a function without the invocation (and its 5-second delay) actually ocurring.",
		Code = "Function.InterceptInvocation( (value1, value2, optional precision as nullable number) => Function.InvokeAfter( () => Value.Equals(value1, value2, precision), #duration(0,0,0,5) ) )",
		Result = "{1,1}"
	] },
	type function (Function as function) as (type function () as (
		type [
			Function = function,
			Parameters = list,
			Result = any
		]
	)),
	(Function as function) as function => let
		UnaryInvocation = (List as list) as record => Record.AddField(
			[
				Function = Function,
				Parameters = List
			],
			"Result",
			() => Function.Invoke(Function, List),
			true
		),
		NewFunction = Function.From(
			Value.Type(Function),
			UnaryInvocation
		)
	in NewFunction
);

shared FunctionParts.Type = type [
	ReturnType = type,
	Parameters = record,
	RequiredParameterCount = Int32.Type
];

shared Type.ForFunctionFromParts = Function.Document(
	"Type.ForFunctionFromParts",
	"Wraps Type.ForFunction such that its <code>min</code> parameter is included as the field <code>RequiredParameterCount</code> in its first parameter, <code>Signature</code>.",
	{ [
		Description = "Dismante a function type into its parts",
		Code = "Type.FunctionParts(type function (A as number, optional B as text) as date)",
		Result = "[ReturnType = type date, Parameters = [A = [Type = number, Optional = false], B = [Type = text, Optional = true]], min = 1]"
	] } ,
	type function (Signature as (
		type [
			ReturnType = type,
			Parameters = record,
			RequiredParameterCount = Int32.Type
		]
	)) as Function.Type,
	(Signature as record) as type => Type.ForFunction(
		Record.RemoveFields(
			Signature,
			{"RequiredParameterCount"},
			Signature[RequiredParameterCount]
	) )
);

shared Function.ReturnNonNull = Function.Document(
	"Function.ReturnNonNull",
	"Modifies a function to return a given non-null value if its result would otherwise be null.",
	{ [
		Description = "Modify <code>Text.StartsWith</code> so that it returns false instead of null when told to check whether <code>null</code> starts with ""ABC"".",
		Code = "Function.ReturnNonNull(Text.StartsWith, false)(null, ""ABC"")",
		Result = "false"
	], [
		Description = "Have Text.StartsWith return ""No Value"" when the text value is null.",
		Code = "Function.ReturnNonNull(Text.StartsWith, true)(""No Value"", ""ABC"")",
		Result = """No Value"""
	] } ,
	type function (Function as function, Value as anynonnull) as function,
	(Function as function, Value as anynonnull) as function => let
		Type = Value.Type(Function),
		Required = Type.FunctionRequiredParameters(Type),
		ReturnType = Type.NonNullable(Type.FunctionReturn(Type)),
		Signature = [
			ReturnType = ReturnType,
			Parameters = Type.FunctionParameters(Type)
		],
		NewFunctionType = Type.ForFunction(Signature, Required),
		NewFunction = (Arguments as list) as anynonnull => let
			Unmodified = Function.Invoke(Function, Arguments),
			Modified =
				if Unmodified = null then
					Value
				else
					Unmodified
		in Modified,
		CheckedTypeCompatibility = if Value.Is(Value, ReturnType) then
			NewFunction
		else
			error Error.Record(
				"Incompatible Type",
				"The type of the value supplied is incompatible with the return type of the function.",
				[
					Function Returns = ReturnType,
					Value Type = Value.Type(Value)
				]
			),
		Metadata = Value.Metadata(Type),
		//Annotate the documentation to disclose the new behaviour.
		NewDocString = "<b>NOTE:</b> Modified to return a value if the result of the function is null. Inspect function metadata to see the value.<p></p>",
		NewDocs = [
			Documentation.Description =
				NewDocString
				& Metadata[Documentation.Description],
			Documentation.LongDescription =
				NewDocString
				& Metadata[Documentation.LongDescription]
		]
	in Function.From(
		NewFunctionType meta ( Metadata & NewDocs & [ Null Replacement = Value ] ),
		CheckedTypeCompatibility
	)
);

shared FunctionSignature.Type = type [
	ReturnType = type,
	Paramters = record,
	//[MyFieldName = RecordField.Type]; RecordField.Type = type [Type = type, Optional = logical]
	min = Int32.Type
];

shared FunctionType.Binary = type function (Left as Any.Type, Right as Any.Type) as Any.Type;

shared FunctionType.NonNullTest = type function (Value as Any.Type) as Logical.Type;

shared FunctionType.Unary = type function (Value as Any.Type) as Any.Type;

shared FunctionType.UnaryTest = type function (Value as Any.Type) as (type nullable Logical.Type);

shared List.Append = Function.Document(
	"List.Append",
	"Add a single element to the tail of a list and return a new list containing the merged result.",
//:: a -> [a] -> [a]
	null,
	type function (Value as any) as (type function (List as list) as list),
	(x as any) => (xs as list) => xs & { x }
);

shared List.Group = Function.Document(
	"List.Group",
	"Works just like <code>Table.Group</code>, but for a list.",
	{ [
		Description = "Group a list of numbers depending on whether they are greater than 3 or not.",
		Code = "List.Group({1..5}, each _ > 3)",
		Result = "{{1,2,3},{4,5}}"
	] } ,
	type function (List as list, Categorizer as (#"FunctionType.Unary"), optional GroupKind as nullable number, optional Comparer as nullable function) as list,
	(List as list, Categorizer as function, optional GroupKind as nullable number, optional Comparer as nullable function) as list => let
		AsTable = Table.FromList(
			List,
			each { _, Categorizer(_) },
			{ "Value", "Group" },
			null,
			null
		),
		Grouped = Table.Group(
			AsTable,
			{"Group"},
			{ {
				"Value",
				each [Value],
				type list
			} },
			GroupKind,
			Comparer
		)[Value],
	AsList = Grouped[Value]
in AsList
);

shared List.Join = Function.Document(
	"List.Join",
	"Given a list a and another list b, create a new list containing the result of appending b to a.",
//:: [a] -> [a] -> [a]
	null,
	type function (#"List A" as list) as (type function (#"List B" as list) as list),
	(a as list) => (b as list) => a & b
);

shared List.MapReduce = Function.Document(
	"List.MapReduce",
	"Maps <code>Iterator</code> across <code>Values</code> and then reduces the result with <code>Reducer</code>.",
	{ [
		Description = "Check whether any value of a list is greater than 3",
		Code = "List.MapReduce({1..5}, each Value.Compare(_, 3) = 1, List.AnyTrue)",
		Result = "true"
	], [
		Description = "Check whether all words in a list are substrings of a sentence.",
		Code = "List.MapReduce({""lazy"", ""fox"", ""dog""}, each Text.Contains(""The quick brown fox jumped over the lazy dog."", _, Comparer.OrdinalIgnoreCase), List.AllTrue)",
		Result = "true"
	], [
		Description = "Calculate the sum of squares of a list of numbers.",
		Code = "List.MapReduce({1..5}, each _ * _, List.Sum)",
		Result = "55"
	] } ,
	type function (
		Values as list,
		Iterator as (
			(type function (Value as any) as any) meta [
				Documentation.FieldDescription = "Acts on each value in Values"
			]
		), Reducer as (
			(type function (List as list) as any) meta [
				Documentation.FieldDescription = "Acts on the iterated list."
			]
		) ) as any,
	(Values as list, Iterator as function, Reducer as function) as any => Reducer(
		List.Transform(
			Values,
			each Iterator(_)
	) )
);

shared List.Nest = Function.Document(
	"List.Nest",
	"Nests each element of a list within a one-element list of its own.",
	{ [
		Description = "Move each element into a list.",
		Code = "List.Nest({1..3})",
		Result = "{{1},{2},{3}}"
	] } ,
	type function (List as list ) as {list}, //,optional Levels as nullable Int64.Type
	(List as list) as list => List.Transform(
		List,
		each { _ })
);

shared List.PadTo = Function.Document(
	"List.PadTo",
	"Appends Padding to List until it reaches Length.",
	{ [
		Description = "Extend a list to length 4",
		Code = "List.PadTo({0},4)",
		Result = "{0,null,null,null}"
	], [
		Description = "Extend a list to length 4 using the value 1",
		Code = "List.PadTo({0}, 4, 1)",
		Result = "{0,1,1,1}"
	] } ,
	type function (List as {list}, Length as Int64.Type, optional Padding as nullable any) as {list},
	(List as list, Length as number, optional Padding as nullable any) as list => List.Last(
		List.Generate(
			() => List,
			each List.Count(_) <= Length,
			each _ & { Padding } 
	) )
);

shared List.Pluck = Function.Document(
	"List.Pluck",
	"Plucks a single value at <code>Position</code> from <code>List</code>.",
	{ [
		Description = "Pluck the item at position 3 from {0,10,20,30,40,50,60,70}",
		Code = "List.Pluck({0,10,20,30,40,50,60,70},3)",
		Result = "30"
	] } ,
	type function (
		List as list,
		Position as Int32.Type,
		optional MissingField as MissingField.Type
	) as any,
	(List as list, Position as number, optional MissingField as number) => Record.Field(
		[
			0 = List{Position},
			2 = List{Position}?,
			1 = error Error.Record(
				"Invalid Paramter Value",
				"MissingField.Ignore is not valid in this function",
				[MissingField = MissingField]
			)
		],
		if MissingField = null then
			MissingField.Error
		else
			MissingField
	)
);

shared List.PluckMany = Function.Document(
	"List.PluckMany",
	"Plucks the items at Positions from List",
	{ [
					Description = "Pluck the items at positions {0,3,6} from {0,10,20,30,40,50,60,70}",
					Code = "List.PluckMany({0,10,20,30,40,50,60,70}, {0,3,6})",
					Result = "{0,30,60}"
	] } ,
	type function (List as list, Positions as {Int32.Type}, optional MissingField as nullable MissingField.Type) as list,
	let
		HandlingOptions = (StartingList as list, Choice as number) => (List as list, Position as number) => let
			Pluck = try StartingList{Position},
			Handling = {
				List & { Pluck[Value] },
				if Pluck[HasError] then
					List
				else
					List
				& { Pluck[Value] },
				List & { if Pluck[HasError] then
					null
				else
					Pluck[Value]
				}
			}{Choice}
		in Handling,
		Implementation = (List as list, Positions as list, optional MissingField as nullable number) as list => List.Accumulate(
			Positions,
			{},
			HandlingOptions(
				List,
				if MissingField = null then
					MissingField.Error
				else
					MissingField
			)
		)
	in Implementation
);

		//:: a -> [a] -> [a]
shared List.Prepend = Function.Document(
	"List.Prepend",
	"Add a single element to the head of a list and return a new list containing the merged result.",
	null,
	type function (Value as any) as (type function (List as list) as list),
	(x as any) => (xs as list) => { x } & xs
);

shared List.ReplaceErrors = List.ReplaceErrorValues;

shared List.ReplaceErrorValues = Function.Document(
	"List.ReplaceErrorValues",
	"Works similarly to <code>Table.ReplaceErrorValues</code>.",
	{ [
		Description = "Replace the errors in a list of numbers with 0",
		Code = "List.ReplaceErrorValues({0,error ""An Error"",2},0)",
		Result = "{0,0,2}"
	] } ,
	type function (List as list, optional Replacement as any) as list,
	(List as list, optional Replacement as any) as list => let
		AsTable = Table.FromValue(List),
		ErrorsReplaced = Table.ReplaceErrorValues(
			AsTable,
			{ {
				"Value",
				Replacement
			} }
		),
		BackToList = ErrorsReplaced[Value]
	in BackToList
);

shared List.RemoveErrors = Function.Document(
	"List.RemoveErrors",
	"Works similarly to <code>Table.RemoveRowsWithErrors</code>.",
	{ [
		Description = "Remove the errors in a list of numbers",
		Code = "List.RemoveErrors({0, error ""An Error"", 2})",
		Result = "{0,2}"
	] } ,
	type function (List as list) as list,
	(List as list) as list => let
		AsTable = Table.FromValue(List),
		ErrorsRemoved = Table.RemoveRowsWithErrors(
			AsTable,
			{"Value"}
		),
		BackToList = ErrorsRemoved[Value]
	in BackToList
);

shared Logical.Imp = let
	NT = (Text as any) => if Text = null then
		"null"
	else
		Text.From(Text),
	Possibilities = { true, false, null },
	Results = { true, false, null, true, true, true, true, null, null },
	SelfJoined = List.TransformMany(
		Possibilities,
		each { Possibilities },
		(L, R) => {
			{ L, L, L },
			R
		}
	),
	Merged = List.Accumulate(
		SelfJoined,
		{
			{},
			{}
		},
		(State, Current) => {
			State{0} & Current{0},
			State{1} & Current{1}
		}
	) & { Results },
	ExampleList = List.Transform(
		List.Zip(Merged),
		each [
			Description = "<code>" & NT(_{0}) & "</code> implies <code>" & NT(_{1}) & "</code>",
			Code = "Logical.Imp(" & NT(_{0}) & ", " & NT(_{1}) & ")",
			Result = NT(_{2})
		]
	),
	TruthTable = Table.FromColumns(
		Merged,
		type table [
			Left = nullable logical,
			Right = nullable logical,
			Result = nullable logical
		]
	)
in Function.Document(
	"Logical.Imp",
	"Performs a logical implication on two expressions.",
	ExampleList,
	type function (Left as nullable logical, Right as nullable logical) as nullable logical,
	(Left as nullable logical, Right as nullable logical) as nullable logical => Table.SingleRow(
		Table.SelectRows(
		TruthTable,
			each ([Left] = Left) and ([Right] = Right)
		)
	)[Result]
);

shared If = #"Logical.If";

shared Logical.If = Function.Document(
	"Logical.If",
	"Select <code>Value</code> if it passes <code>Test</code>, otherwise return <code>Alternative</code>, which defaults to <i>null</i>.",
	{ [
		Description = "Choose 4 if greater than 7, otherwise 8",
		Code = "Logical.If(4, each _ > 7, 8)",
		Result = "8"
	], [
		Description = "Choose 4 if greater than 2, otherwise 8",
		Code = "Logical.If(4, each _ > 2, 8)",
		Result = "4"
	] } ,
	type function (// Considered an Otherwise option, decided against, can implement in the test function as (Value) => try Test(try Value otherwise Otherwise) otherwise true
		Value as any,
		Test as (#"FunctionType.NonNullTest"),
		Else as ( type any meta [ Documentation.FieldDescription = "<i>Optional</i> Use instead of Value if <code>Test(Value>=false</code>" ] )
	) as any,
	(Value as any, Test as function, Else as any) => let
		_Test = Test(Value)
	in if (
		if _Test = null then
			false
		else
			_Test
	)
	then
		Value
	else
		Else
);

/* // A bad idea?
Logical.Ifs = let
	Documentation = [
		Documentation.Name = "Logical.Ifs",
		Documentation.Description = "Evaluates a sequence of conditions.",
		Documentation.Examples = {[
			Description = "Evaluates <code>if (if 0 > 1 then 0 else 2) - 100 < 0 then <i>that result</i> else 99</code>.",
			Code = "Logical.Ifs({[Selector = each _, Test = each > 1, 2],[Selector = each _ -100, Test = each < 1, 99]}, 0)"
		]}
	],
	IfType = type [
		Selector = (#"UnaryFunction.Type"),
		Test = (#"UnaryNonNullTest.Type"),
		Else = any
	],
	Type = type function (
		Ifs as ({IfType})
	) as logical,
	Implementation = (Ifs as list) as logical => (Seed as any) => List.Accumulate(
		Ifs, Seed, (State, If) => #"Logical.If"(If[Selector](State), If[Test], If[Else])
	)
in Value.ReplaceType(Implementation, Type meta Documentation)
,
*/

shared Logical.Not = Function.Document(
	"Logical.Not",
	"Changes true to false and false to true.",
	{ [
		Description = "Change True to False",
		Code = "Logical.Not(True)",
		Result = "False"
	] } ,
	type function (Logical as Logical.Type) as logical,
	(Logical as logical) as logical => not Logical
);

shared Logical.Xor = Function.Document(
	"Logical.Xor",
	"Returns the result of a logical XOR (exclusive or) on two logical values.",
	{ [
		Description = "Returns true because only one of the two values is true.",
		Code = "Logical.Xor(true, false)",
		Result = "true"
	],[
		Description = "Returns <code>false</code> because both values are true.",
		Code = "Logical.Xor(true, true)",
		Result = "false"
	] } ,
	type function (
		Value1 as (type nullable Logical.Type meta [ Documentation.FieldCaption = "Value 1" ]),
		Value2 as (type nullable Logical.Type meta [ Documentation.FieldCaption = "Value 2" ])
	) as nullable Logical.Type,
	(Value1 as nullable logical, Value2 as nullable logical) as nullable logical => let
		Either = Value1 or Value2,
		Both = Value1 and Value2,
		Result = if (Value1 = null or Value2 = null) then
			null
		else
			Either and not Both
	in Result
);

shared Number.IsInteger = Function.Document(
	"Number.IsInteger",
	"Determines whether <code>number</code> is an integer.",
	{ [
		Description = "-4 is an integer",
		Code = "Number.IsInteger(-4)",
		Result = "true"
	], [
		Description = "1.72 is not an integer",
		Code = "Number.IsInteger(1.72)",
		Result = "false"
	] } ,
	type function (Number as nullable number) as nullable logical,
	(Number as nullable number) as nullable number => Number.Mod(Number, 1) = 0
);

/* Use facets, and work out numeric precision ranges.
	Number.Integer =
		let
			//Read https://bengribaudo.com/blog/2020/02/28/5009/power-query-m-primer-part17-type-system-ii-facets
			FunctionDocumentation = [
				Documentation.Name = "Number.Integer",
				Documentation.Description = "Returns the integer portion of <code>number</code>.",
				Documentation.Examples = {
					[
						Description = "Find the integer portion of -4.38",
						Code = "Number.Integer(-4.38)",
						Result = "-4"
					]
				}
			],
			FunctionType = type function (Number as number) as number,
			FunctionImplementation =
				(Number as number) as number =>
					let
						Integer =
							Number.Round(
								Number - Number.Mod(Number, 1),
								0
							),
						Type =
							List.Select(
								{
									{
										-x,
										x,
										Int8.Type
									},
									{
										-x,
										x,
										Int16.Type
									},
									{
										-x,
										x,
										Int32.Type
									},
									{
										-x,
										x,
										Int64.Type
									}
								}
							),
						Result = Value.ReplaceType(Integer, Type)
					in
						Result
		in
			Value.ReplaceType(
				FunctionImplementation,
				FunctionType meta FunctionDocumentation
			),
*/

shared Record.RemoveNullValues = Function.Document(
	"Record.RemoveNullValues",
	"Removes all null fields from the record.",
	{ [
		Description = "Remove null values from a record.",
		Code = "Record.RemoveNulls([A=1,B=2,C=null,D=4])",
		Result = "[A=1,B=2,D=4]"
	] } ,
	type function (Record as record) as record,
	(Record as record) as record => List.Accumulate( {
		each
		List.Zip( {
			Record.FieldNames(_),
			Record.FieldValues(_)
		} ),
		each List.Select(_, each _{1} <> null),
		each List.Transform(
			_,
			each Record.FromList({_{0}}, {_{1}})
		),
		Record.Combine
	},
		Record,
		(State, Current) => Current(State)
	)
);

shared Record.SelectWhere = Function.Document(
	"Record.SelectWhere",
	"Select fields from <code>Record</code> if the field value meets a criterion.",
	{ [
		Description = "Select text fields from a record where the field names starts with B",
		Code = "Record.SelectWhere([A = 1, B = ""Barry"", C = ""Charlie""], (Name, Value) => Text.StartsWith(Name, ""B"") and Type.Is(Value.Type(Value), type text))",
		Result = "[B = ""Barry""]"
	] } ,
	let
		SelectionFunction = type function (#"Field Name" as Text.Type, #"Field Value" as Any.Type) as Logical.Type,
		Type = type function (Record as record, #"Selection Function" as SelectionFunction) as record
	in Type,
	(Record as record, SF as function) as record => List.Accumulate( {
		each List.Zip( {
			Record.FieldNames(_),
			Record.FieldValues(_)
		} ),
		each List.Select(_, each SF(_{0}, _{1})),
		each List.Transform(_, List.First),
		each Record.SelectFields(Record, _)
	},
		Record,
		(State, Current) => Current(State)
	)
);

shared Record.TransformAll = Function.Document(
	"Record.TransformAll",
	"Apply the same function to all the fields of Record.",
	{ [
		Description = "Add 1 to each field.",
		Code = "Record.TransformAll([A=1,B=8], each _ + 1)",
		Result = "[A=2,B=9]"
	] } ,
	type function (Record as record, Function as function) as record,
	(Record as record, Function as function) as record => let
		FieldNames = Record.FieldNames(Record),
		Result = Record.TransformFields(
			Record,
			List.Transform(
				FieldNames,
				each { _, Function }
			)
		)
	in Result
);

shared RecordField.Type = type [
	Type = type,
	Optional = logical
];

shared RecordFields.Type = type {#"RecordField.Type"};

shared Table.AddColumns = Function.Document(
	"Table.AddColumns",
	"Adds multiple Columns to a tables where Columns contains lists containing the new column name, generation function, and optionally its type.",
	{ [
		Description = "Add two new columns to a table named and containing uppercase and lowercase letters, respectively.",
		Code = "Table.AddColumns(#table(type table [X = number], {{1},{2}}), {{""Y"", each ""y"", type text}, {""Z"", each ""z""}})",
		Result = "#table(type table [X = number, Y = text, Z = any], {{1, ""y"", ""z""},{2, ""y"", ""z""}})"
	] } ,
	type function (Table as table, Columns as {list}) as table,
	(Table as table, Columns as list) as table => List.Accumulate(
		Columns,
		Table,
		(state as table, current as list) as table => Table.AddColumn(
			state,
			current{0},
			current{1},
			try current{2} otherwise Type.FunctionReturn(Value.Type(current{1}))
		)
	)
);

/*
	 * Provide a simplified method for programmatically contructing table from
	 * relationships between columns. Accepts a list of lists of the structure:
	 *
	 *     {<column name>, <column generator>, <type>}
	 *
	 * The chain may then be fed a table containing at least one column as a
	 * seed.
	 *
	 * :: (Table a, Record b) => [(Text, b -> c, Type)] -> a -> a
//AddColumns = Compose(ChainOperations, Map(Cons(Table.AddColumn)))
//just work this into Table.AddColumns
*/

shared Table.ApplyColumnSchema = Function.Document(
	"Table.ApplyColumnSchema",
	"Applies a Column Schema Table to Table.",
	{ [
		Description = "An Example:",
		Code = "Table.ApplyColumnSchema(#table({""Column1"",""Column2""}, {{1,1.3},{2,1.1}}), #table(type table [Old Name = nullable text, New Name = nullable text, Type = nullable type, Operation = nullable function], {{""Column1"", ""ID"", Int64.Type, null}, {""Column2"", ""COUPON"", Percentage.Type, each Value.Divide(_, 100)}}))",
		Result = "#table(type table [ID = Int64.Type, COUPON = Percentage.Type], {{1, 0.013}, {2, 0.011}})"
	] } ,
	type function (Table as table, #"Column Schema Table" as table, optional MissingField as nullable number) as table,
	(Table as table, #"Column Schema Table", optional MissingField as nullable number) as table => let
		ColumnPair = (Col1 as text, Col2 as text) as list => List.Accumulate( {
			each Table.SelectColumns(
				_,
				{ Col1, Col2 }
			),
			each Table.SelectRows(
				_,
				each not ( Record.Field(_, Col1) is null or Record.Field(_, Col2) is null )
			),
			Table.ToColumns,
			List.Zip
		},
			#"Column Schema Table",
			(State, Current) => Current(State)
		),
		Steps = {
			each Table.RenameColumns(
				_,
				ColumnPair("Old Name", "New Name"),
				MissingField
			),
			each Table.TransformColumns(
				_,
				ColumnPair("New Name", "Operation"),
				null,
				MissingField
			),
			each Table.TransformColumnTypes(
				_,
				List.Select(
					ColumnPair("New Name", "Type"),
					(Pair as list) => List.Contains(
						Table.ColumnNames(_),
						Pair{0}
					)
				)
			)
		},
		Result = List.Accumulate(
			Steps,
			Table,
			(State, Current) => Current(State)
		)
	in Result
);

shared Table.AssignTextTypes = Function.Document(
	"Table.AssignTextTypes",
	"Transforms column types to match a specified <code>Table Type</code> and assigns any leftover columns <code>type text</code>.",
	{ [
		Description = "Transform a table type and underlying data.",
		Code = "Table.AssignTextTypes(#table(type table [A = text, B = text, C = text, D = text, E = text], {{""1"",""11"", ""2020/01/01"", ""2020-01-02 00:00:00.000"", ""12""},{""2"",""022"", ""2020/01/02"", ""2020-02-02 00:00:00.000"", ""34""}}), type table [A = Int64.Type, B = nullable text, C = date, D = datetime])",
		Result = "#table(type table [A = Int64.Type, B = nullable text, C = date, D = datetime, E = text], {{1, ""11"", #date(2020,1,1), #datetime(2020,1,2,0,0,0,0), ""12""},{2,""022"", #date(2020,1,2), #datetime(2020,2,2,0,0,0,0), ""34""}})"
	] } ,
	type function (Table as table, #"Table Type" as type) as table,
		//This is copied from the Record.SelectWhere function. Your current dynamic M-loading code doesn't seem to support use of non-#shared formulas. You had come up with some sort of recursive process that evaluated until the error disappeared at HSBC. See if you can create a proper M extension.
	(Table as table, Type as type) as table => let
		TypeRecordFields = Type.RecordFields(Type.TableRow(Type)),
		ColumnNames = Table.ColumnNames(Table),
		TypedColumns = Record.FieldNames(TypeRecordFields),
		TextColumns = Record.FieldNames(
			#"Record.SelectWhere"(
				TypeRecordFields,
				(Name, Value) =>
					Type.Is(Value[Type], type nullable text)
					or Type.Is(Value[Type], type text)
			)
		),
		ValuFromTxt = Table.TransformColumns(
			Table,
			List.Transform(
				List.Difference(
					TypedColumns,
					TextColumns
				),
				each { _, Value.FromText }
			)
		),
		AssignTypes = Table.EnforceType(ValuFromTxt, Type)
	in AssignTypes
);

shared Table.ColumnSchemaTable = Function.Document(
	"Table.ColumnSchemaTable",
	"Creates a Column Schema Table from a list of lists that contain, in order, the original column name, the new column name, the column type, and any operation to apply to the column.",
	{ [
		Description = "Create a column schema that will rename the columns, divide COUPON by 100, and then apply the data type.",
		Code = "Table.ColumnSchemaTable({{""Column1"", ""ID"", Int64.Type}, {""Column2"", ""COUPON"", Percentage.Type, each Value.Divide(_, 100)}})",
		Result = "#table(type table [Old Name = nullable text, New Name = nullable text, Type = nullable type, Operation = nullable function], {{""Column1"", ""ID"", Int64.Type, null}, {""Column2"", ""COUPON"", Percentage.Type, each Value.Divide(_, 100)}})"
	] },
	type function (#"Column Schema" as {list}) as table,
	(Schema as list) as table => let
		AsTable = #table(
			type table [
				Old Name = nullable text,
				New Name = nullable text,
				Type = nullable type,
				Operation = nullable function
			],
			List.Transform(
				Schema,
				each #"List.PadTo"(_, 4)
			)
//TODO: Allow for shorter lists by checking whether list lengths are short, whether 2nd column are all types, etc.
		),
		KeptOldNames = Table.TransformRows(
			AsTable,
			each if [New Name] = null then
				_ & [ New Name = [Old Name] ]
			else
				_
		),
		Recombined = Table.FromRecords(KeptOldNames),
		ReTyped = Value.ReplaceType(
			Recombined,
			Value.Type(AsTable)
		)
	in ReTyped
);

shared Table.CompareColumnNames = Function.Document(
	"Table.CompareColumnNames",
	"Presents a table showing whether column names match among tables.",
	{ [
		Description = "Show column name similarities among 3 tables",
		Code = "Table.CompareColumnNames({#table({""A"",""B"",""D"",""E""},{}),#table({""A"",""C"",""D""},{}),#table({""D"",""E"",""F""},{})})",
		Result = "#table({""Table1"",""Table2"",""Table3""},{{""A"",""A"",null},{""B"",null,null},{null, ""C"",null},{""D"",""D"",""D""},{""E"",null,""E""},{null,null,""F""}})"
	] } ,
	type function (Tables as {table}) as table,
	(Tables as list) as table => let
		NameLists = List.Transform(
			Tables,
			each List.Sort(Table.ColumnNames(_))
		),
		Recursion = (Result, NameLists) => let
			Heads = List.Transform(
				NameLists,
				each List.First(_, null)
			),
			Min = List.Min(Heads),
			Matches = List.PositionOf(
				Heads,
				Min,
				Occurrence.All
			),
			SkipOrKeep = List.Transform(
				{0..List.Count(Tables) - 1},
				each if List.Contains(Matches, _) then
					{ List.Skip, Min }
				else
					{ Value.NoOp, null }
			),
			NextLists = List.Transform(
				List.Zip( {
					List.Transform(
						SkipOrKeep,
						each _{0}
					),
					NameLists
				} ),
				each _{0}(_{1})
			),
			Result = if List.AllTrue(
				List.Transform(
					NameLists,
					each List.IsEmpty(_)
				)
			) then Result
			else
				@Recursion(
					Result
					& List.Transform(SkipOrKeep, each _{1}),
					NextLists
				)
			in Result,
			RecursionResult = Recursion({}, NameLists),
		Result = Value.ReplaceType(
			Table.FromRows(
				List.Split(
					RecursionResult,
					List.Count(Tables)
				)
			),
			type table
			Type.FromList(
				List.Transform(
					{1..List.Count(Tables)},
					each "Table" & Number.ToText(_)
				),
				type nullable text
			)
		)
	in Result
);

// Note: Does not allow multiple tables to have the same column names. Not a bug. Won't fix.
shared Table.Crossjoin = Function.Document(
	"Table.Crossjoin",
	"Crossjoins table contents.",
	{ [
		Description = "Combine 3 tables",
		Code = "Table.Crossjoin({X,Y,Z})",
		Result = "Table.FromRecords(XYZ)"
	] } ,
	type function (Tables as {table}) as table,
	(Tables as list) as table => let
		CrossjoinTables = (CrossjoinedTables, CurrentTable) => Table.ExpandTableColumn(
			Table.AddColumn(
				CrossjoinedTables,
				"CrossjoinTable",
				each CurrentTable,
				Value.Type(CurrentTable)
			),
			"CrossjoinTable",
			Table.ColumnNames(CurrentTable)
		),
		Result = List.Accumulate(
				List.Skip(Tables),
				List.First(Tables),
				CrossjoinTables
			)
		in Result
);

shared Table.EnforceType = Function.Document(
	"Table.EnforceType",
	"Applies <code>Table Type</code> to <code>Table</code> and also converts underlying data.",
	null,
	type function (Table as table, #"Table Type" as type) as table,
	(Table as table, Type as type) as table => let
		RecordFields = Type.RecordFields(Type.TableRow(Type)),
//TODO: Need to handle ÷100 for % and Date/Datetime and raise error if binary
		ColumnTypes = List.Transform(
			Record.FieldValues(RecordFields),
			each [Type]
		),
		Transformations = List.Zip( {
			Record.FieldNames(RecordFields),
			ColumnTypes
		} ),
		TransformedTable = Table.TransformColumnTypes(
			Table,
			Transformations
		)
	in TransformedTable
);

shared Table.FillByGroups = Function.Document(
	"Table.FillByGroups",
	"Splits a table into groups and then fills down specified columns to fill in data gaps only within the specified groups." ,
	null,
	type function (Table as table, Groups as {text}, FillDowns as {text}) as table,
	(Table as table, Groups as list, FillDowns as list) as table => Table.Combine(
		List.Transform(
			Table.SplitByGroups(Table, Groups),
			each Table.FillDown(_, FillDowns)
		)
	)
);

shared Table.NameColumns = Function.Document(
	"Table.NameColumns",
	"Like Table.RenameColumns, but assigns Names from left to right after Skip columns instead of requiring the matching column names.",
	{ [
		Description = "Rename the first 2 columns of a table.",
		Code = "Table.NameColumns(#table(type table [Column1 = any, Column2 = any, Column3 = any], {}), {""A"",""B""})",
		Result = "#table(type table [A = any, B = any, Column3 = any], {})"
	],[
		Description = "Rename the first 2 columns of a table.",
		Code = "Table.NameColumns(#table(type table [Column1 = any, Column2 = any, Column3 = any], {}), {""B""})",
		Result = "#table(type table [Column1 = any, B = any, Column3 = any], {})"
	] },
	type function (Table as table, #"Column Names" as {text}, optional Skip as nullable Int64.Type) as table,
	(Table as table, Names as list, optional Skip as number) as table => Table.RenameColumns(
		Table,
		List.Zip( {
			List.FirstN(
				List.Skip(
					Table.ColumnNames(Table),
					if Skip is null then
						0
					else
						Skip
				),
			List.Count(Names)
			),
			Names
		} )
	)
);

shared Table.PickHeaders = let
	ExampleTable = "#table(type table [A = number, B = text, C = date, D = text], {{1,""B1"",#date(2020,1,1),""abcde""},{2,""B2"",#date(2020,2,2),""fghij""}})",
	AfterPromotion = ", {{2,""B2"",#date(2020,2,2),""fghij""}})",
//TODO: Documentation.LongDescription = ... explain the selector (Header, Value) signature and that takes Value if evaluates to true, and that it will fail if it causes two columns to have the same name.
	Examples = { [
		Description = "Apply default <code>Table.PromoteHeaders</code> behaviour.",
		Code = "Table.PickHeaders(" & ExampleTable & ")",
		Result = "#table(type table [1 = number, B1 = text, Column3 = date, abcde = text]" & AfterPromotion
	],[
		Description = "Promote only text values longer than 3 characters.",
		Code = "Table.PickHeaders(" & ExampleTable & ", (Header, Value) => try Text.Length(Value) > 3 otherwise false)",
		Result = "#table(type table [A = number, B = text, C = date, abcde = text]" & AfterPromotion
	],[
		Description = "Promote only the columns as specified in a list.",
		Code = "Table.PickHeaders(" & ExampleTable & ", {0,2,""D""})",
		Result = "#table(type table [1 = number, B = text, #""2020-01-01"" = date, abcde = text]" & AfterPromotion
	],[
		Description = "This function will not promote values of type null or those that cannot be converted to text via the <code>Text.From</code> function.",
		Code = "Table.PickHeaders(#table(type table [A = nullable number, B = number], {{null,1,""C1"",()=>1},{2,3,""C2"",()=>2}}), {0..3})",
		Result = "#table(type table [A = nullable number, 1 = number, C1 = text, D = function], {{2,3,""C2"",()=>2}})"
	] }
in Function.Document(
	"Table.PickHeaders",
	"Promote values of the first row only as determined by a function or selection list.",
		//TODO: Add code to check/enforce this type: Selector = type function (Header as text, Value as any) as logical,
	Examples,
	type function (Table as table, optional Selector as any) as table,
	let 
		PromotionPossible = (Value as any) as logical => not (
			(try Text.From(Value))[HasError]
			or (Value = null)
		),
		ChooseViaList =
			(Choices as list, Selector as list) => List.Transform(
				Choices,
				each if List.ContainsAny(
					Selector,
					{
						[Index],
						[Header]
					}
				) and PromotionPossible([Value]) then
					Text.From([Value])
				else
					[Header]
			),
		ChooseViaFunction = (Choices as list, Selector as function) as list => List.Transform(
			Choices,
			each if Selector([Header], [Value]) and PromotionPossible([Value]) then
				Text.From([Value])
			else
				[Header]
			),
		Implementation = (Table as table, optional Selector as any) => let
			FirstRowValues = Record.FieldValues(Table.First(Table)),
			Choices = List.Generate(
				() => 0,
				each _ < Table.ColumnCount(Table),
				each _ + 1,
				each [
					Index = _,
					Header = Table.ColumnNames(Table){_},
					Value = FirstRowValues{_}
				]
			),
			Result = if Value.Is(Selector, type logical) or (Selector = null) then
				Table.PromoteHeaders( Table, [PromoteAllScalers = Selector] )
			else
				Table.Skip(
					Table.NameColumns(
						Table,
						if Value.Is(Selector, type function) then
							ChooseViaFunction(Choices, Selector)
						else if Value.Is(Selector, type list) then
							ChooseViaList(Choices, Selector)
						else
							error Error.Record(
								"Incorrect Value Type",
								"Selector must be null, logical, function, or list"
							)
						)
					)
		in Result
	in Implementation
);

shared Table.PromoteHeadersOfType = Function.Document(
	"Table.PromoteHeadersOfType",
	"Promotes the values of the first row to headers, but only where the type matches a type in <code>Types</code>.",
	{ [
		Description = "Promote only text values.",
		Code = "Table.PromoteHeadersOfType(#table(type table [A=text, B=number,C=date], {{""A1"",1,#date(2020,1,1)},{""A2"",2,#date(2020,2,2)}}), {type text})",
		Result = "#table(type table [A1=text,B=number,C=date], {{""A2"",2,#date(2020,2,2)}})"
	] } ,
	type function (Table as table, Types as {type}) as table,
	(Table as table, Types as list) as table => Table.PickHeaders(
		Table,
		(Value as any) as logical => List.AnyTrue(
			List.Transform(
				Types,
				each Value.Is(Value, _)
			)
		)
	)
);

shared Table.SelectAvailableColumns = Function.Document(
	"Table.SelectAvailableColumns",
	"Like <code>Table.SelectColumns</code>, but silently ignores the absence of any columns not in <code>Table</code>.",
	{ [
		Description = "Select columns A and C from a table that has only columns A and B, without an error.",
		Code = "Table.SelectAvailableColumns(#table(type table [A = any, B = any], {}), {""A"",""C""})",
		Result = "#table(type table [A = any], {}))"
	] } ,
	type function (Table as table, #"Column Names" as {text}) as table,
	(Table as table, Columns as list) as table => Table.SelectColumns(
		Table,
		List.Intersect( {
			Table.ColumnNames(Table),
			Columns
		} )
	)
);

shared Table.SelectColumnNumbers = Function.Document(
	"Table.SelectColumnNumbers",
	"Select a set of table columns by zero-base ordinal position.",
	{ [
		Description = "Select columns from position 1 to 3",
		Code = "Table.SelectColumnNumbers(#table(type table [A = number, B = number, C = number, D = number, E = number], {}), {1..3})",
		Result = "#table(type table [B = number, C = number, D = number], {})"
	] } ,
	type function (Table as table, Positions as {Int32.Type}, optional MissingField as nullable MissingField.Type) as table,
	(Table as table, Positions as list, optional MissingField as nullable number) as table => Table.SelectColumns(
		Table,
		List.PluckMany(
			Table.ColumnNames(Table),
			Positions,
			MissingField
		)
	)
);

shared Table.SelectColumnsWhere = Function.Document(
	"Table.SelectColumnsWhere",
	"Select columns that satisfy a condition on each column name.",
	{ [
		Description = "Select columns whose names do not begin with <i>Column</i>.",
		Code = "Table.SelectColumnsWhere(#table(type table [A = number, Column2 = number], {}), each not Text.StartsWith(_, ""Column""))",
		Result = "#table(type table [A = number])"
	],[
		Description = "Select columns whose minimum value exceeds 4",
		Code = "let Table = #table(type table [A = number, B = number], {{8,1},{9,3},{6,2}}), Result = Table.SelectColumnsWhere(Table, each List.Min(Table.Column(Table, _)) > 4) in Result",
		Result = "#table(type table [A = number], {{8},{9},{6})"
	] } ,
	let
		ColumnSelector = type function (#"Column Name" as Text.Type) as Logical.Type
	in type function (Table as table, Selector as (ColumnSelector)) as table,
	(Table as table, Selector as function) as table => List.Accumulate( {
		Table.ColumnNames,
		(ColumnNames) => List.Generate(
			() => 0,
			each _ < Table.ColumnCount(Table),
			each _ + 1,
			each [
				Name = ColumnNames{_},
				Selected = Selector(ColumnNames{_})
			]
		),
		each List.Select(_, each [Selected]),
		each List.Transform(_, each [Name]),
		each Table.SelectColumns(Table, _)
	},
		Table,
		(State, Current) => Current(State)
	)
);

shared Table.SplitByGroups = Function.Document(
	"Table.SplitByGroups",
	"Splits a table into groups.",
	null,
	type function (Table as table, Groups as {text}) as {table},
	let
		Accumulator = (GUID as text) => (Tables as list, Group as text) => List.Combine(
			List.Transform(
				Tables,
				each Table.Column(
					Table.Group(
						_,
						Group,
						{ { GUID, each _, Value.Type(_) } },
						GroupKind.Global
					),
					GUID
				)
			)
		),
		Implementation = (Table as table, Groups as list) as list => List.Accumulate(
			Groups,
			{Table},
			Accumulator(Text.NewGuid())
		)
	in Implementation
);

shared Table.Unlayer = Function.Document(
	"Table.Unlayer",
	"Converts the rows of Table into records and merges them into a single row.",
	{ [
		Description = "Transform a 3 row table into a record.",
		Code = "Table.Unlayer(#table(""A"", ""B"", {{1,2},{3,4}}))",
		Result = "#table(""Row0.A"", ""Row0.B"", ""Row1.A"", ""Row1.B"", {{1,2,3,4}})"
	] },
	type function (Table as table) as record,
	(Table as table) as record => Record.Combine(
		List.Transform(
			List.Zip( {
				Table.ToRecords(Table),
				List.Generate(
					() => 0,
					each _ < Table.RowCount(Table),
					each _ + 1,
					each "Row" & Text.From(_) & "."
				)
			} ),
			(Row) => Record.RenameFields(
				Row{0},
				List.Transform(
					Record.FieldNames(Row{0}),
					each { _, Row{1} & _ }
				)
			)
		)
	)
);

//Possible BUG: what happens when Character is not a single character?
shared Text.CondenseCharacters = Function.Document(
	"Text.TrimInside",
	"Replaces any consecutive repetition of <code>Character</code> in <code>Text</code> with a single <code>Character</code>.",
	{ [
		Description = "Remove multiple extra spaces separating words within text.",
		Code = "Text.TrimInside(""A  B"")",
		Result = "A B"
	],[
		Description = "Replace consecutive B's with a single B",
		Code = "Text.TrimInside(""ABBA"", ""B"")",
		Result = "ABA"
	] } ,
	type function (Text as text, optional Character as text) as text,
	(Text as text, optional Character as text) as text => Text.Start(
		if Character is null then
			Text
		else (
			if Text.StartsWith(Text, Character) then
				Character
			else
				""
		) & Text.Combine(
			List.Select(
				Text.Split(Text, Character),
				each _ <> ""
			),
			Character
		) & (
			if Text.EndsWith(Text, Character) then
				Character
			else
				""
			),
		Text.Length(Text)
	)
);

shared Text.CondenseSpaces = Function.Document(
	"Text.CondenseSpaces",
	"Replaces consecutive spaces with a single space and trims the result.",
	{ [
		Description = "Eliminate repeated spaces.",
		Code = "Text.CondenseSpaces(""A  B"")",
		Result = "A B"
	] } ,
	type function (Text as nullable text) as nullable text,
	(Text as nullable text) as nullable text => Text.Trim(#"Text.CondenseCharacters"(Text, " "))
);

shared Text.ContainsAny = Function.Document(
	"Text.ContainsAny",
	"Like Text.Contains, but tests against any one of multiple text strings passed as a list.",
	{ [
		Description = "Test whether a text string contains any of multiple other text strings.",
		Code = "Text.ContainsAny(""ABCDE"",{""AC"",""B"",""DE""})",
		Result = "true"
	] } ,
	type function (Text as text, #"Text List" as {text}, optional Comparer as nullable function) as logical,
	(Text as text, TextList as list, optional Comparer as nullable function) as logical => List.Generate(
		() => false,
		each (List.Count(_) > List.Count(TextList)) or List.Last(_),
		each Text.Contains(
			Text,
			TextList{List.Count(_)},
			if Comparer = null then
				Value.Equals
			else
				Comparer
		)
	)
);

shared Text.NullifyZeroLength = Function.Document(
	"Text.NullifyZeroLength",
	"Changes a zero-length string like """" into <code>null</code>.",
	{ [
		Description = "Change """" into <code>null</code>.",
		Code = "Text.NullifyZeroLength("""")",
		Result = "<code>null</code>"
	] } ,
	type function (Text as nullable text) as nullable text,
	(Text as nullable text) as nullable text => if Text = "" then
		null
	else
		Text
);

shared Type.AbstractTypes = Function.Document(
	"Type.AbstractTypes",
	"Returns the list of abstract M data types.",
	{ [
		Description = "The list of abstract M data types.",
		Code = "Type.AbstractTypes()",
		Result = "{Function.Type,Table.Type,None.Type,Any.Type}"
	] } ,
	type function () as (type {type}),
	() as list => {
		Function.Type,
		Table.Type,
		Any.Type,
		None.Type
	}
);

shared Type.ForTable = Function.Document(
	"Type.ForTable",
	"Creates a table type with columns named Names and of type Types",
	{ [
		Description = "Create a table with a text column called Text and a numeric column called Numbers.",
		Code = "Type.ForTable({""Text"", ""Numbers""}, {type text, type number}",
		Result = "type table [Text = text, Numbers = number]"
	] },
	type function (Names as {text}, Types as {type}) as type,
	(Names as list, Types as list) as type => let
		ColumnTypes = List.Transform(
			Types,
			each Record.FromList(
				{ _, false },
				RecordField.Type
			)
		),
		TableDefinition = Record.FromList(ColumnTypes, Names),
		TableType = type table Type.ForRecord(TableDefinition, false)
	in TableType
);

// TODO: Add error checking to verify Types is a type or a list of types equal in length to List, or implement MissingField...
shared Type.FromList = Function.Document(
	"Type.FromList",
	"Create a record type to suit a given list of labels and types.",
	{ [
		Description = "Create a record type for two values having the same type.",
		Code = "Type.FromList({""A"",""B""},type text)",
		Result = "type [A = [Type = text, Optional = false], B = [Type = text, Optional = false]]"
	],[
		Description = "Create a record type for two values having the different types.",
		Code = "Type.FromList({""A"",""B""},{type text, type number})",
		Result = "type [A = [Type = text, Optional = false], B = [Type = number, Optional = false]]"
	] } ,
	type function (List as list, Types as any) as type,
	(List as list, Types as any) as any => let
		DefaultType = if Value.Is(Types, type type) then
			Types
		else if Value.Is(Types, type {type}) and (List.Count(Types) = List.Count(List)) then
			type any
		else
			error Error.Record("Invalid Arguments", null),
		_Types = if Value.Is(Types, type {type}) then
			Types
		else
			List.Repeat(
				{DefaultType},
				List.Count(List)
			),
		RecordTypes = List.Transform(
			_Types,
			each Record.FromList( {
				_,
				false
			},
			RecordField.Type
			)
		),
		RowType = Record.FromList(RecordTypes, List),
		Result = Type.ForRecord(RowType, false)
	in Result
);

shared Type.FunctionParts = Function.Document(
	"Type.FunctionParts",
	"Returns a record containing <code>Function Type</code>'s <code>ReturnType</code>, <code>Parameters</code>, and <code>RequiredParameterCount</code>.",
	{ [
		Description = "Dismante a function type into its parts",
		Code = "Type.FunctionParts(type function (A as number, optional B as text) as date)",
		Result = "[ReturnType = type date, Parameters = [A = [Type = number, Optional = false], B = [Type = text, Optional = true]], RequiredParameterCount = 1]"
	] } ,
	type function (#"Function Type" as Function.Type) as FunctionParts.Type,
	(FunctionType as type) as record => let
		ParameterFields = Type.FunctionParameters(FunctionType),
		Result = Record.FromList(
			{
				Type.FunctionReturn(FunctionType),
				Type.FunctionParameters(FunctionType),
				Type.FunctionRequiredParameters(FunctionType)
			},
			FunctionParts.Type
		),
		Return = if Type.Is(FunctionType, type function) then
			Result
		else
			error Error.Record(
				"Expression.Error",
				"FunctionType is not of type function.",
				[FunctionType = FunctionType]
			)
	in Return
);

shared Type.PrimitiveTypes = Function.Document(
	"Type.PrimitiveTypes",
	"Returns a list of all primitive M data types.",
	{ [
		Description = "The list of primitives.",
		Code = "Type.Primitives()",
		Result = "{Binary.Type, Date.Type, DateTime.Type, DateTimeZone.Type, Duration.Type, List.Type, Logical.Type, Null.Type, Number.Type, Record.Type, Text.Type, Time.Type, Type.Type}"
	] } ,
	type function () as (type {type}),
	() as list => {
		Binary.Type,
		Date.Type,
		DateTime.Type,
		DateTimeZone.Type,
		Duration.Type,
		//Note that List.Type is not abstract because it represents {any}, or any possible list.
		List.Type,
		Logical.Type,
		Null.Type,
		Number.Type,
		//Note that Record.Type is not abstract because it represents [...], or any possible record
		Record.Type,
		Text.Type,
		Time.Type,
		Type.Type
	}
);

shared Type.ToText = Function.Document(
	"Type.ToText",
	"Returns a textual representation of <code>Type</code>.",
	{ [
		Description = "Return",
		Code = "Type.ToText(type number)",
		Result = """number"""
	] } ,
	type function (Type as type, optional Nullable as nullable logical) as text,
	let
		TypeTable = #table(
				type table [
					Type = type,
					Type Name = text
				],
				{
					{ type binary, "binary" },
					{ type date, "date" },
					{ type datetime, "datetime" },
					{ type datetimezone, "datetimezone" },
					{ type duration, "duration" },
					{ type table, "table" },
					{ type time, "time" },
					{ type record, "record" },
					{ type list, "list" },
					{ type number, "number" },
					{ type function, "function" },
					{ type logical, "logical" },
					{ type text, "text" }
				}
			),
		Implementation = (Type as type, optional Nullable as nullable logical) as text => (
			if ( if Nullable = null then false else Nullable ) and Type.IsNullable(Type) then
				"nullable "
			else
				""
			) & Table.SingleRow(
				Table.SelectRows(
					TypeTable,
					each Type.Is([Type], Type)
				)
			)
			[Type Name]
	in Implementation
);

//This snippit should be moved into a separate function. It's tricky.
//    RecordType = Record.FromList({type nullable text, false}, type [Type = type, Optional = logical]),
//Record.SelectNames = null,
// Value.ReplaceType( (Rcd as record, optional Criteria as any, optional Comparer as function) as record =>
/*
Record.SelectValues = (Rcd as record) => let
	Defaults = [Comparer = Comparer.Equals],
	Comparer = null,
	Cmp = if Value.Is(Comparer, type null) then Defaults[Comparer] else Comparer,
	Result = Apply({
		Record.ToTable,
		(tbl) => Table.SelectRows(tbl, each [Value] = List.Min(tbl[Value]))
})
in Result
/*
, type function (
	Rcd as record,
	Comparer as function
) as record meta [
	Documentation.Name = "",
	Documentation.LongDescription = "",
	Documentation.Examples = {[
		Description = "",
		Code = "",
		Result = ""
	]}
])
,
*/


shared Value.ConformsToType = Function.Document(
	//TODO: Don't check function name equivalence by default.
	"Value.ConformsToType",
	"Determines whether <code>Value</code> conforms to <code>type</code>, including custom types and their sub-type components recursively until resolving their primative types.",
	{ [
		Description = "<code>[a=1,b=""two"",c=#date(2021,1,1)]</code> is of type <code>type [a = number, b = text, ...]</code>",
		Code = "Value.ConformsToType([a=1,b=""two"",c=#date(2021,1,1)], type [a = number, b = text, ...])",
		Result = "true"
	],[
		Description = "<code>[a=1,b=""two"",c=#date(2021,1,1)]</code> is not of type <code>type [a = number, b = text]</code>",
		Code = "Value.ConformsToType([a=1,b=""two"",c=#date(2021,1,1)], type [a = number, b = text])",
		Result = "false"
	],[
		Description = "<code>1</code> is of type <code>type number</code>",
		Code = "Value.ConformsToType(1, type number)",
		Result = "true"
	] } ,
		/*TODO: Show more advanced usage (functions, sub-types, etc.)
Use this for unit testing and auto-generation of documentation.
let
	ShouldConform = List.Transform({
		{0, type number},
		{{0..1}, type {number}},
		{[], type record},
		{[A=1], type [A = number]},
		{[A=1,B=2], type [A=number, ...]},
		{[A=1,B=[C=1,D={[E=1],[E=1,F=2],[E=1,F=2,G=3]}]], type [A=number,B=[C=number,D={(type [E=number,...])}]]},
		{() => ..., type function () as any},
		{(A as number, B as record) as logical => ..., type function (A as number, B as record) as logical}
	}, each _ & {true}),
	ShouldNotConform = List.Transform({
		{0, type text},
		{{0..1}, type {text}},
		{[], type text},
		{[A=1], type [B = number]},
		{[A=1,B=2], type [A=text, ...]},
		{[A=1,B=[C=1,D={[E=1],[E=1,F=2],[E=1,F=2,G=3]}]], type [A=number,B=[C=number,D={(type [E=number])}]]},
		{() => ..., type function () as anynonnull},
		{(A as number, B as record) as nullable logical => ..., type function (A as number, B as record) as logical}
	}, each _ & {false}),
	Tests = Table.FromRows(ShouldConform & ShouldNotConform, type table [Value = any, Type = type, Should Conform = logical]),
	#"Invoked Function" = Table.AddColumn(Tests, "Conforms", each Function.Invoke(Value_ConformsToType, List.FirstN(Record.FieldValues(_),2)), Logical.Type),
	#"Conformity Check" = Table.AddColumn(#"Invoked Function", "As Expected", each [Should Conform] = [Conforms], type logical),
	#"Added Index" = Table.AddIndexColumn(#"Conformity Check", "Index", 1, 1, Int64.Type),
	#"Failed Tests" = Table.SelectRows(#"Added Index", each ([As Expected] = false))
in
	#"Failed Tests"
			*/
		/*
	To implement Depth (i.e. check conformity only to a given depth) you'll need a closure to hold the original Depth parameter, verify it is an integer, decriment it on each call of GoDeeper, but not when going through a list of record fields from Type.RecordFeilds (for that, see if Function.ScalarFunction helps call it only once?)
	To implement Match Names (defaults to true as is, you'll just have to )
	Options = type [
		optional Depth as Int32.Type,
		optional Match Names as (Logical.Type meta [Documentation.FieldDescription = "Match record (and function parameter) names, not only their types by ordinal position."
	],
	OptionDefaults = Record.FromList({null, false}, Options),
	*/
	type function (Value as Any.Type, Type as Type.Type/*,
		optional Options as Options */
		// Provide the option to inspect number facets, table keys? - No, those aren't types, so make separate functions for those rather than further complicating this one.
	) as Logical.Type,
	(Value as any, Type as type/*, optional Options as record*/ ) as logical => let
	Recursion = (Value as any, Type as type) as logical => let //This additional layer is necessary to evaluate this shared function as an M expression (rather than only within the section document, which works without it) because otherwise GoDeeper would recurse on Value.ConformsToType, which doesn't exist when evaluated, because the record name comes out as 'shared Value.ConformsToType', and so doesn't exist.
		//Value_ConformsToType = (Value as any, Type as type, MaxDepth as nullable number, Depth as number) => let
		PrimitiveConformsToType = (Type as type) as logical => Value is type or Value.Is(Value, Type),
		GoDeeper = (Value as any, Type as type) => @Recursion(Value, Type),
		//, if Depth = null then null else Depth - 1),
		TypeConformsToType = let
			//TypeConformsToListType = {GoDeeper(Type.ListItem(Value), Type.ListItem(Type))},
			TypeConformsToListType = {
				Function.Invoke(
					GoDeeper,
					List.Transform(
						{ Value, Type },
						Type.ListItem
					)
				)
			},
			TypeConformsToTableType = GoDeeper(Type.TableRow(Value), Type),
			TypeConformsToRecordType = let
				ValueRecordFields = Type.RecordFields(Value),
				TypeRecordFields = Type.RecordFields(Type),
				Checks = {
					{
						each Record.FieldCount(Type.RecordFields(_)),
						Value.Equals
					},{
						each Record.FieldNames(Type.RecordFields(_)),
						Value.Equals
					},{
						Type.IsOpenRecord,
						(ValueIsOpen, TypeIsOpen) => Logical.Imp(ValueIsOpen, TypeIsOpen)
					},{
						each List.Transform(
							Record.FieldValues(Type.RecordFields(_)),
							each [Optional]
						),
						Value.Equals
					},{
						each List.Transform(
							Record.FieldValues(Type.RecordFields(_)),
							each [Type]
						),
						(Value, Type) => List.AllTrue(
							List.Transform(
								List.Zip( { Value, Type } ),
								each Function.Invoke(Type.Is, _)
							)
						)
					}
				},
				Checked = List.TransformMany(
					{ { Value, Type } },
					each List.Transform(
						Checks,
						each Record.FromList(
							_,
							type [
								Check = function,
								Comparer = function
							]
						)
					),
					(Types, Checks) => Function.Invoke(
						Checks[Comparer],
						List.Transform(
							Types,
							each Checks[Check](_)
						)
					)
				)
			in Checked,
			TypeConformsToFunctionType = let
					TypeParameters = (Type as type) => Type.FunctionParameters(Type),
					FunctionParameters = TypeParameters(Value),
					FunctionTypeParameters = TypeParameters(Type),
					Conforms = {
						Type.Is(
							Type.FunctionReturn(Value),
							Type.FunctionReturn(Type)
						),
						Type.FunctionRequiredParameters(Value) = Type.FunctionRequiredParameters(Type)
					} & List.Transform(
						List.Zip( {
							Record.FieldValues(FunctionParameters),
							Record.FieldValues(FunctionTypeParameters)
						} ),
						each GoDeeper(_{0}, _{1})
					),
					FunctionTypeConformsToType = if
						Record.FieldNames(FunctionParameters) = Record.FieldNames(FunctionTypeParameters) then
							Conforms
						else
							{ false }
				in FunctionTypeConformsToType,
			TypeConformanceResults = List.SingleOrDefault(
					List.Select( {
						{
							List.Type,
							TypeConformsToListType
						},{
							Table.Type,
							TypeConformsToTableType
						},{
							Record.Type,
							TypeConformsToRecordType
						},{
							Function.Type,
							TypeConformsToFunctionType
						}
					},
					each Type.Is(Value, _{0})
				),
				{
					null,
					{ Type.Is(Value, Type) }
				}
			){1},
			Conforms = TypeConformanceResults
		in Conforms,
		ValueConformsToListType = List.Transform(
			Value as list,
			each GoDeeper(_, Type.ListItem(Type))
		),
		ValueConformsToTableType = if GoDeeper(Value.Type(Value), Type) then
			List.Transform(
				Table.ToRecords(Value),
				each GoDeeper(_, Type.TableRow(Type))
			)
		else
			{ false },
		ValueConformsToRecordType = let
			TypeFields = Type.RecordFields(Type),
			TypeFieldNames = Record.FieldNames(TypeFields),
			HasExcessFields = not List.IsEmpty(
				List.Difference(
					Record.FieldNames(Value),
					TypeFieldNames
				)
			),
			ValuesConformToFieldTypes = List.TransformMany(
				TypeFieldNames,
				(TypeFieldName) => {try Record.Field(Value, TypeFieldName)},
				(TypeFieldName, RecordFieldValueLookupAttempt) => let
					TypeField = Record.Field(TypeFields, TypeFieldName),
					Conforms = if RecordFieldValueLookupAttempt[HasError] then
						//Possible bug: *maybe* Conforms should be enclosed in a list
						TypeField[Optional]
					else
						GoDeeper(
							RecordFieldValueLookupAttempt[Value],
							TypeField[Type]
						)
				in Conforms
			),
		FieldsConformToType = ValuesConformToFieldTypes & { Type.IsOpenRecord(Type) or not HasExcessFields }
	in FieldsConformToType,
	ValueConformsToFunctionType = { GoDeeper(Value.Type(Value), Type) },
	ValueConformanceResults = List.SingleOrDefault(
		List.Select( { {
			Type.Type,
			TypeConformsToType
		},{
			List.Type,
			ValueConformsToListType
		},{
			Table.Type,
			ValueConformsToTableType
		},{
			Record.Type,
			ValueConformsToRecordType
		},{
			Function.Type,
			ValueConformsToFunctionType
		} },
		each Value.Is(Value, _{0})
	),
		{ null, { PrimitiveConformsToType(Type) } }
	){1},
	Conforms = if PrimitiveConformsToType(Type) then
		ValueConformanceResults
	else
		{ false }
	in List.AllTrue(Conforms)
in Recursion
);

shared Value.NonNull = Function.Document(
	"Value.NonNull",
	"Takes a non-null value to be returned by a new unary function if the value it takes equals <i>null</i>.",
	{ [
		Description = "Create an unary function that will return false if the value it takes as an argument is null.",
		Code = "Value.NonNull(false)(null)",
		Result = "false"
	] } ,
	type function (NonNullValue as anynonnull) as function,
	(NonNullValue as anynonnull) as function => (Value as any) as anynonnull => if Value = null then
		NonNullValue
	else
		Value
);

shared Value.NoOp = Function.Document(
	"Value.NoOp",
	"Does nothing. This function simply passes an input value unaltered.",
	{ [
		Description = "Pass 7, return the same value unaltered.",
		Code = "Value.NoOp(7)",
		Result = "7"
	] } ,
	type function (Value as any) as any,
	(Val as any) as any => Val
);

shared Value.Repeat = Function.Document(
	"Value.Repeat",
	"Repeats <code>Value</code> <code>Repetitons</code> times.",
	{ [
		Description = "Repeat the number 7 three times.",
		Code = "Value.Repeat(7,3)",
		Result = "{7,7,7}"
	] } ,
	type function (Value as any, Repetitions as Int32.Type) as {any},
	(Value as any, Repetitions as number) as list => Value.ReplaceType(
		List.Generate(
			() => 0,
			each _ < Repetitions,
			each _ + 1,
			each Value
		),
		type {Value.Type(Value)}
	)
);