using System.Text;

namespace day_x;

internal static class Day13
{
	public static void Solve()
	{
		var rows = InputReader.ReadRows("Day13.txt");
		Part1(rows);
		Part2(rows);
	}

	static void Part1(string[] rows)
	{
		var chunks = SplitChunks(rows);
		var res = chunks.Select(FindReflection1).Sum();
		Console.WriteLine($"Total sum is: {res}");
	}

	static void Part2(string[] rows)
	{
		var chunks = SplitChunks(rows);
		var res = chunks.Select(FindReflection2).Sum();
		Console.WriteLine($"Total sum is: {res}");
	}

	static int FindReflection1(List<string> chunk) =>
		FindReflection(chunk, CheckReflection);

	static int FindReflection2(List<string> chunk) =>
		FindReflection(chunk, CheckReflectionWithSmudge);

	static int FindReflection(
		List<string> chunk,
		Func<List<string>, int, bool> checkReflection,
		bool isTransposed = false)
	{
		for (int i = 1; i < chunk.Count; i++)
		{
			if (checkReflection(chunk, i))
			{
				return isTransposed ? i : i * 100;
			}

		}

		var transposed = Transpose(chunk);
		return FindReflection(transposed, checkReflection, true);
	}

	static bool CheckReflection(List<string> chunk, int midpoint)
	{
		var j = midpoint - 1;

		for (int i = midpoint; i < chunk.Count; i++)
		{
			if (j < 0)
			{
				return true;
			}

			if (chunk[i] != chunk[j])
			{
				return false;
			}

			j--;
		}

		return true;
	}

	private static bool CheckReflectionWithSmudge(List<string> chunk, int midpoint)
	{
		var diff = 0;
		var j = midpoint - 1;
		for (int i = midpoint; i < chunk.Count; i++)
		{
			if (j < 0)
			{
				return diff == 1;
			}
			diff += CalculateDiff(chunk[i], chunk[j]);
			j--;
		}

		return diff == 1;

		int CalculateDiff(string one, string two) =>
			one.Zip(two)
			.Select(pair => pair.First == pair.Second)
			.Count(x => !x);
	}

	static List<List<string>> SplitChunks(string[] rows)
	{
		var chunks = rows.Aggregate(
			new List<List<string>> { new() },
			(acc, curr) =>
			{
				if (curr == "")
				{
					acc.Add([]);
				}
				else
				{
					acc[^1].Add(curr);
				}

				return acc;
			});

		return chunks;
	}

	static List<string> Transpose(List<string> rows)
	{
		var transposed = new List<string>();

		for (int j = 0; j < rows[0].Length; j++)
		{
			var row = new StringBuilder();
			for (int i = 0; i < rows.Count; i++)
			{
				row.Append(rows[i][j]);
			}

			transposed.Add(row.ToString());
		}

		return transposed;
	}
}
