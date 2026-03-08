using System.Data;
using System.Numerics;

namespace day_x;

internal static class Day12
{
	private const char DAMAGED = '#';
	private const char OPERATIONAL = '.';
	private const char UNKNOWN = '?';

	private static Dictionary<(string InputString, string GroupString, int CurrentDamagedBlockSize), BigInteger> Memo = new();

	public static void Solve()
	{
		var rows = InputReader.ReadRows("Day12.txt");

		var result1 = Solve(ParseInput1(rows));
		Console.WriteLine(result1);

		var result2 = Solve(ParseInput2(rows));
		Console.WriteLine(result2);
	}

	private static BigInteger Solve(List<Springs> rows)
	{
		BigInteger result = 0;

		foreach (var row in rows)
		{
			result += SolveRow(row);
		}

		return result;
	}

	// row --->			{ InputString = ???.###, GroupsString = 1,1,3 }
	private static BigInteger SolveRow(Springs row, int currentDamagedBlockSize = 0)
	{
		if (Memo.TryGetValue(CalculateCacheKey(row, currentDamagedBlockSize), out var cached))
		{
			return cached;
		}

		BigInteger result = 0;

		if (row.InputString == "") // we have consumed all of the input string.
		{
			var validEmpty = row.Groups.Length == 0 && currentDamagedBlockSize == 0;                    // there must be no more groups to match and no unmatched current block.
			var validLastGroup = row.Groups.Length == 1 && currentDamagedBlockSize == row.Groups[0];    // or the last group matches the current block.
			result = validEmpty || validLastGroup ? 1 : 0;
			Memo.Add(CalculateCacheKey(row, currentDamagedBlockSize), result);
			return result;
		}

		char[] firstSymbolCandidates = row.InputString[0] == UNKNOWN ? [DAMAGED, OPERATIONAL] : [row.InputString[0]];

		foreach (var firstSymbol in firstSymbolCandidates)
		{
			if (firstSymbol == DAMAGED) // if damaged, increase the currentDamagedBlock and keep going.
			{
				var newBlockSize = currentDamagedBlockSize + 1;
				result += SolveRow(row with { InputString = row.InputString[1..] }, newBlockSize);
			}
			else  // if operational, we need to finish the current damaged block.
			{
				if (currentDamagedBlockSize > 0)
				{
					if (row.Groups.Length > 0 && row.Groups[0] == currentDamagedBlockSize) // if we have a damaged block, check if it matches the size of the first remaining group, if any.
					{
						result += SolveRow(new Springs { InputString = row.InputString[1..], Groups = row.Groups[1..] });
					}
				}
				else // otherwise, skip the current character and keep going.
				{
					result += SolveRow(row with { InputString = row.InputString[1..] }, 0);
				}
			}
		}

		Memo.Add(CalculateCacheKey(row, currentDamagedBlockSize), result);
		return result;
	}

	private static (string InputString, string, int currentDamagedBlockSize) CalculateCacheKey(Springs row, int currentDamagedBlockSize) =>
		(row.InputString, string.Join(',', row.Groups), currentDamagedBlockSize);

	// row --->			????.######..#####. [1,6,5]
	private static List<Springs> ParseInput1(string[] rows)
	{
		var res = new List<Springs>();
		foreach (var row in rows)
		{
			var splitted = row.Split(' ');
			var value = splitted[0];
			var groupsString = splitted[1];
			var groups = groupsString.Split(',').Select(int.Parse).ToArray();
			res.Add(new(value, groups));
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
			res.Add(new(value, groups));
		}
		return res;
	}


	record struct Springs(string InputString, int[] Groups);
}
