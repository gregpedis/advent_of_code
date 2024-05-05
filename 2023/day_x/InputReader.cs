using System.Data.Common;

namespace day_x;

internal static class InputReader
{
	private const string PREFIX = """E:\repos\playground_haskell\2023\day_x\input""";

	public static string ReadRaw(string filename)
	{
		var fullPath = Path.Combine(PREFIX, filename);
		return File.ReadAllText(fullPath);
	}

	public static string[] ReadRows(string filename)
	{
		var fullPath = Path.Combine(PREFIX, filename);
		return File.ReadAllLines(fullPath);
	}

	public static string[] ReadColumns(string filename)
	{
		var rows = ReadRows(filename);

		var colLength = rows[0].Length;
		var columns = Enumerable.Range(0,colLength)
			.Select(colIndex => new string(rows.Select(row => row[colIndex]).ToArray()));

		return columns.ToArray();
	}
}
