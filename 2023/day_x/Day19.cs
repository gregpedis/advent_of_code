using System.Data;
using System.Numerics;

namespace day_x;

internal static class Day19
{
	private const string REJECTED = "R";
	private const string ACCEPTED = "A";
	private const string INITIAL = "in";
	private const int LIMIT = 4000;

	public static void Solve()
	{
		var rows = InputReader.ReadRows("Day19.txt");
		var data = ParseInput(rows);
		Console.WriteLine(Solve1(data));
		Console.WriteLine(Solve2(data));
	}

	private static InputData ParseInput(string[] rows)
	{
		var splitAt = rows.ToList().FindIndex(string.IsNullOrWhiteSpace);
		var parts = ParseParts(rows[(splitAt + 1)..]);
		var workflows = ParseWorkflows(rows[0..splitAt]);
		return new(workflows, parts);
	}

	private static int Solve1(InputData data)
	{
		return data.Parts.Select(x => Calculate(x, data.Workflows)).Sum();

		static int Calculate(Part part, Workflow[] workflows)
		{
			var workflow = workflows.First(x => x.Id == INITIAL);
			var workflowId = workflow.Id;

			while (true)
			{
				workflowId = ProgressWorkflow(part, workflow);
				if (workflowId is REJECTED or ACCEPTED)
				{
					break;
				}
				else
				{
					workflow = workflows.First(x => x.Id == workflowId);
				}
			}

			return workflowId == REJECTED
				? 0
				: part.X + part.M + part.A + part.S;
		}

		static string ProgressWorkflow(Part part, Workflow workflow)
		{
			foreach (var rule in workflow.Rules)
			{
				if (rule.condition is null || Evaluate(rule))
				{
					return rule.Next;
				}
			}
			throw new NotImplementedException();

			bool Evaluate(Rule rule) =>
				rule.condition.GreaterThan
					? rule.condition.Property(part) > rule.condition.Value
					: rule.condition.Property(part) < rule.condition.Value;
		}
	}

	private static BigInteger Solve2(InputData data)
	{
		var qq = new Queue<(Ranges, string)>();
		var initial = new Ranges(
			new(1, LIMIT),
			new(1, LIMIT),
			new(1, LIMIT),
			new(1, LIMIT)
			);
		qq.Enqueue((initial, INITIAL));

		BigInteger result = 0;
		while (qq.TryDequeue(out var current))
		{
			var (ranges, workflowId) = current;
			if (ranges is null || !ranges.IsValid)
			{
				continue;
			}
			if (workflowId == ACCEPTED)
			{
				result += ranges.Permutations;
			}
			else if (workflowId != REJECTED)
			{
				var workflow = data.Workflows.First(x => x.Id == workflowId);
				var rest = ranges;
				foreach (var rule in workflow.Rules)
				{
					var (r, rs) = PartitionByRule(rest, rule);
					qq.Enqueue((r, rule.Next));
					rest = rs;
				}
			}
		}

		return result;

		static (Ranges, Ranges) PartitionByRule(Ranges ranges, Rule rule)
		{
			if (rule.condition is null)
			{
				return (ranges with { }, null!);
			}

			// x> 50
			if (rule.condition.GreaterThan)
			{
				if (rule.condition.PropertyName == 'x')
				{
					return (
						ranges with { X = AtLeast(ranges.X, rule.condition.Value + 1) },
						ranges with { X = AtMost(ranges.X, rule.condition.Value) }
						);
				}
				else if (rule.condition.PropertyName == 'm')
				{
					return (
						ranges with { M = AtLeast(ranges.M, rule.condition.Value + 1) },
						ranges with { M = AtMost(ranges.M, rule.condition.Value) }
						);
				}
				else if (rule.condition.PropertyName == 'a')
				{
					return (
						ranges with { A = AtLeast(ranges.A, rule.condition.Value + 1) },
						ranges with { A = AtMost(ranges.A, rule.condition.Value) }
						);
				}
				else
				{
					return (
						ranges with { S = AtLeast(ranges.S, rule.condition.Value + 1) },
						ranges with { S = AtMost(ranges.S, rule.condition.Value) }
						);
				}
			}
			// x< 50
			else
			{
				if (rule.condition.PropertyName == 'x')
				{
					return (
						ranges with { X = AtMost(ranges.X, rule.condition.Value - 1) },
						ranges with { X = AtLeast(ranges.X, rule.condition.Value) }
						);
				}
				else if (rule.condition.PropertyName == 'm')
				{
					return (
						ranges with { M = AtMost(ranges.M, rule.condition.Value - 1) },
						ranges with { M = AtLeast(ranges.M, rule.condition.Value) }
						);
				}
				else if (rule.condition.PropertyName == 'a')
				{
					return (
						ranges with { A = AtMost(ranges.A, rule.condition.Value - 1) },
						ranges with { A = AtLeast(ranges.A, rule.condition.Value) }
						);
				}
				else
				{
					return (
						ranges with { S = AtMost(ranges.S, rule.condition.Value - 1) },
						ranges with { S = AtLeast(ranges.S, rule.condition.Value) }
						);
				}
			}

			Range AtLeast(Range range, int value) =>
				range with { From = Math.Max(range.From, value) };

			Range AtMost(Range range, int value) =>
				range with { To = Math.Min(range.To, value) };
		}
	}

	// EXAMPLE: {x=787,m=2655,a=1222,s=2876}
	private static Part[] ParseParts(string[] values)
	{
		return values.Select(Parse).ToArray();

		static Part Parse(string value)
		{
			var splitted = value[1..^1].Split(",");
			var x = int.Parse(splitted[0].Split("=")[1]);
			var m = int.Parse(splitted[1].Split("=")[1]);
			var a = int.Parse(splitted[2].Split("=")[1]);
			var s = int.Parse(splitted[3].Split("=")[1]);
			return new(x, m, a, s);
		}
	}

	// EXAMPLE: qqz{s>2770:qs,m<1801:hdj,R}
	private static Workflow[] ParseWorkflows(string[] values)
	{
		new WorkflowParser(values[0]).Parse();
		return values.Select(x => new WorkflowParser(x).Parse()).ToArray();
	}

	// Part 1
	record class InputData(Workflow[] Workflows, Part[] Parts);
	record class Part(int X, int M, int A, int S);
	record class Workflow(string Id, Rule[] Rules);
	record class Rule(string Next, Condition condition = null);
	record class Condition(Func<Part, int> Property, char PropertyName, bool GreaterThan, int Value);
	// Part 2
	record class Ranges(Range X, Range M, Range A, Range S)
	{
		public BigInteger Permutations =>
			BigInteger.Multiply(X.To - X.From + 1, M.To - M.From + 1)
			* (A.To - A.From + 1)
			* (S.To - S.From + 1);

		public bool IsValid =>
			X.To >= X.From
			&& M.To >= M.From
			&& A.To >= A.From
			&& S.To >= S.From;
	}
	record class Range(int From, int To);

#pragma warning disable S1643 // Strings should not be concatenated using '+' in a loop
	private sealed class WorkflowParser
	{
		readonly string _value;
		int _index;

		public WorkflowParser(string value)
		{
			_value = value;
			_index = 0;
		}

		public Workflow Parse()
		{
			var id = Id();
			var rules = Rules();
			return new(id, rules);
		}

		private string Id()
		{
			var id = "";
			while (_value[_index] != '{')
			{
				id += _value[_index];
				_index++;
			}
			_index++; // consume '{'
			return id;
		}

		private Rule[] Rules()
		{
			var rules = new List<Rule>
			{
				Rule()
			};
			if (MoreRules() is { } moreRules)
			{
				rules.AddRange(moreRules);
			}
			return [.. rules];
		}

		private Rule Rule()
		{
			var condition = Condition();
			var next = Next();
			return new Rule(next, condition);
		}

		private Condition Condition()
		{
			var nextComma = _value.IndexOf(',', _index);
			var colon = _value.IndexOf(':', _index);
			if (colon < 0)
			{
				return null!;
			}
			else if (nextComma < 0 || colon < nextComma)
			{
				var condition = _value[_index..colon];
				_index = colon + 1; // consume ':'

				var propertyName = condition[0];
				var @operator = condition[1];
				var value = int.Parse(condition[2..]);

				Func<Part, int> accessor = propertyName switch
				{
					'x' => part => part.X,
					'm' => part => part.M,
					'a' => part => part.A,
					's' => part => part.S,
					_ => throw new NotImplementedException()
				};

				return @operator switch
				{
					'<' => new(accessor, propertyName, false, value),
					'>' => new(accessor, propertyName, true, value),
					_ => throw new NotImplementedException()
				};
			}
			else
			{
				return null!;
			}
		}

		private string Next()
		{
			var next = "";
			while (_value[_index] is not ',' and not '}')
			{
				next += _value[_index];
				_index++;
			}
			return next;
		}

		private Rule[] MoreRules()
		{
			if (_value[_index] == ',')
			{
				_index++; // consume ','
				var rules = new List<Rule> { Rule() };

				if (MoreRules() is { } more)
				{
					rules.AddRange(more);
				}
				return rules.ToArray();
			}
			else
			{
				return null!;
			}
		}
	}
}
