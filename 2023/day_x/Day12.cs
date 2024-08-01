namespace day_x;

internal static class Day12
{
	private const char DAMAGED = '#';
	private const char OPERATIONAL = '.';
	private const char UNKNOWN = '?';

	private static readonly Dictionary<int, List<string>> Memoized = new();

	private static readonly Dictionary<string, List<string>> Cache = [];

	public static void Solve()
	{
		var rows = InputReader.ReadRows("Day12.txt");

		var result1 = Solve(ParseInput1(rows));
		Console.WriteLine(result1);

		var result2 = Solve(ParseInput2(rows));
		Console.WriteLine(result2);
	}

	private static int Solve(List<Springs> rows)
	{
		return rows.Select(SolveRow).Sum();
	}

	private static int SolveRow(Springs row)
	{
		var permutations = Permutations(row.Value);
		var valid = permutations.Count(x => x == row.GroupsString);
		return valid;
	}

	// ???.### 1,1,3
	private static bool TryCalculatePrefix(string springs, int damagedCount, out List<string> suffixes)
	{
		suffixes = [];
		foreach (var prefix in Memoize(damagedCount))
		{
			if (springs.StartsWith(prefix))
			{
				suffixes.Add(springs[prefix.Length..]);
			}
			if (springs.EndsWith(prefix)
				&& !springs[..^prefix.Length].Contains(DAMAGED))
			{
				suffixes.Add(string.Empty);
			}

			// MAYBE PAD IT with . at the beginning and end?
			// need to make sure that when we pad at the end, we don't return it as a suffix.
			// Or does it even matter?

			// skip operational springs
			var start = Math.Min(springs.IndexOf(DAMAGED), springs.IndexOf(UNKNOWN));
			for (var i = start; i < springs.Length - prefix.Length; i++)
			{

			}
		}
		return suffixes.Count > 0;
	}

	private static List<string> Memoize(int damagedCount)
	{
		if (Memoized.TryGetValue(damagedCount, out var cached))
		{
			return cached;
		}
		var res = new List<string>();
		for (int i = 0; i < damagedCount; i++)
		{
			res.AddRange(Memoize(damagedCount - 1).Select(x => DAMAGED + x));
			res.AddRange(Memoize(damagedCount - 1).Select(x => UNKNOWN + x));
		}
		Memoized[damagedCount] = res;
		return res;
	}

	private static List<string> Permutations(string row)
	{
		if (Cache.TryGetValue(row, out List<string>? value))
		{
			return value.Select(CalculateGroups).ToList();
		}
		var res = new List<string>();

		var temp = new Stack<string>();
		temp.Push(row);
		while (temp.TryPop(out var current))
		{
			if (current.Contains(UNKNOWN))
			{
				foreach (var next in Explode(current))
				{
					temp.Push(next);
				}
			}
			else
			{
				res.Add(current);
			}
		}

		Cache[row] = res.ToList();
		return Cache[row].Select(CalculateGroups).ToList();

		// Fix the unknown characters one at a time
		static IEnumerable<string> Explode(string row)
		{
			var unknown = row.IndexOf(UNKNOWN);
			if (unknown >= 0)
			{
				yield return row[..unknown] + DAMAGED + row[(unknown + 1)..];
				yield return row[..unknown] + OPERATIONAL + row[(unknown + 1)..];
			}
			else
			{
				yield return row;
			}
		}
	}

	// .??..??...?##. 1,1,3
	// ?#?#?#?#?#?#?#? 1,3,1,6
	private static string CalculateGroups(string row)
	{
		var groups = new List<int>();

		var current = 0;
		foreach (var x in row)
		{
			if (x == DAMAGED)
			{
				current++;
			}
			else if (current > 0)
			{
				groups.Add(current);
				current = 0;
			}
		}
		if (current > 0)
		{
			groups.Add(current);
		}
		return string.Join(',', groups);
	}

	private static List<Springs> ParseInput1(string[] rows)
	{
		var res = new List<Springs>();
		foreach (var row in rows)
		{
			var splitted = row.Split(' ');
			var value = splitted[0];
			var groupsString = splitted[1];
			var groups = groupsString.Split(',').Select(int.Parse).ToArray();
			res.Add(new(value, groupsString, groups));
		}
		return res;
	}

	private static List<Springs> ParseInput2(string[] rows)
	{
		var res = new List<Springs>();
		foreach (var row in rows)
		{
			var splitted = row.Split(' ');
			var value = string.Join("?", Enumerable.Repeat(splitted[0], 5));
			var groupsString = string.Join(",", Enumerable.Repeat(splitted[1], 5));
			var groups = groupsString.Split(',').Select(int.Parse).ToArray();
			res.Add(new(value, groupsString, groups));
		}
		return res;
	}

	record struct Springs(string Value, string GroupsString, int[] Groups);
}
