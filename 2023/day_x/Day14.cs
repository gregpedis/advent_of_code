using System.Runtime.InteropServices;
using System.Text;

namespace day_x;

internal static class Day14
{
	const char ROCK = 'O';
	const char EMPTY = '.';
	const char STOP = '#';
	const int CYCLES = 1_000_000_000;

	public static void Solve()
	{
		var states = new List<string[]>();

		var rows = InputReader.ReadRows("Day14.txt");

		var fixedRows = rows;

		for (int j = 0; j < CYCLES; j++)
		{
			for (int i = 0; i < 4; i++)
			{
				fixedRows = Tilt(fixedRows);
				fixedRows = RotateRight(fixedRows);
			}

			if (LoopDetected(states, fixedRows, out var loopStart))
			{
				CalculateFinishTotalLoad(states, loopStart);
				break;
			}

			states.Add(fixedRows);
		}
	}

	private static void CalculateFinishTotalLoad(List<string[]> states, int loopStart)
	{
		for (int i = loopStart; i < states.Count; i++)
		{
			Console.WriteLine($"State {i}: {TotalLoad(states[i])}");
		}

		var index = (CYCLES - loopStart) % (states.Count - loopStart) + loopStart - 1;
		var result = TotalLoad(states[index]);
		Console.WriteLine($"TOTAL LOAD HAS SAME LOAD AS INDEX: {index}");
		Console.WriteLine($"TOTAL LOAD IS: {result}");
	}

	private static bool LoopDetected(List<string[]> states, string[] current, out int i)
	{
		for (i = 0; i < states.Count; i++)
		{
			if (Equals(states[i], current))
			{
				Console.WriteLine("Loop found");
				Console.WriteLine($"Loop start: {i}");
				Console.WriteLine($"Loop end: {states.Count}");
				Console.WriteLine($"Loop length: {states.Count - i}");
				return true;
			}
		}

		i = -1;
		return false;
	}

	static string FixColumn(string column)
	{
		var empty = 0;
		var rocks = 0;

		var result = new StringBuilder();

		foreach (var c in column)
		{
			switch (c)
			{
				case EMPTY:
					empty++;
					break;

				case ROCK:
					if (empty == 0)
					{
						result.Append(ROCK);
					}
					else
					{
						rocks++;
					}
					break;

				case STOP:
					result.Append(Enumerable.Repeat(ROCK, rocks).ToArray());
					result.Append(Enumerable.Repeat(EMPTY, empty).ToArray());
					result.Append(STOP);
					rocks = empty = 0;
					break;
			}
		}
		result.Append(Enumerable.Repeat(ROCK, rocks).ToArray());
		result.Append(Enumerable.Repeat(EMPTY, empty).ToArray());
		return result.ToString();
	}

	static string[] Tilt(string[] rows)
	{
		var columns = Transpose(rows);
		var fixedColumns = columns.Select(FixColumn).ToArray();
		return Transpose(fixedColumns);
	}

	static string[] Transpose(string[] rows)
	{
		var colLength = rows[0].Length;
		var columns = Enumerable.Range(0, colLength)
			.Select(colIndex => new string(rows.Select(row => row[colIndex]).ToArray()));

		return columns.ToArray();
	}

	static string[] RotateRight(string[] rows)
	{
		var rotated = new List<string>();

		for (int j = 0; j < rows.Length; j++)
		{
			var row = "";
			for (int i = rows.Length - 1; i >= 0; i--)
			{
				row += rows[i][j];
			}
			rotated.Add(row);
		}

		return rotated.ToArray();
	}

	static int TotalLoad(string[] rows)
	{
		var totalLoad = rows
			.Select((row, index) =>
				(rows.Length - index) * row.Count(x => x == 'O'))
			.Sum();
		return totalLoad;
	}

	static bool Equals(string[] state1, string[] state2)
	{
		for (int i = 0; i < state1.Length; i++)
		{
			if (state1[i] != state2[i])
			{
				return false;
			}
		}
		return true;
	}
}

