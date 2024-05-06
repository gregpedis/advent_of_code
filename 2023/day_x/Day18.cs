using System.Numerics;

namespace day_x;

internal static class Day18
{
	public static void Solve()
	{
		var rows = InputReader.ReadRows("Day18.txt");

		var part1 = Shoelace(ParseInput1(rows));
		Console.WriteLine($"Total surface area: {part1}");

		var part2 = Shoelace(ParseInput2(rows));
		Console.WriteLine($"Total surface area: {part2}");
	}

	static BigInteger Shoelace(List<Instruction> instructions)
	{
		(BigInteger, BigInteger) previous = (0, 0);

		BigInteger inside = 0;
		BigInteger outside = 0;

		foreach (var instr in instructions)
		{
			var next = instr.Heading switch
			{
				Direction.Up => (previous.Item1 - instr.Steps, previous.Item2),
				Direction.Down => (previous.Item1 + instr.Steps, previous.Item2),
				Direction.Right => (previous.Item1, previous.Item2 + instr.Steps),
				Direction.Left => (previous.Item1, previous.Item2 - instr.Steps),
				_ => throw new NotImplementedException("wtf"),
			};

			inside += previous.Item1 * next.Item2 - previous.Item2 * next.Item1;
			outside += instr.Steps;
			previous = next;
		}

		return (BigInteger.Abs(inside) + outside) / 2 + 1;
	}

	static List<Instruction> ParseInput1(string[] instructions)
	{
		var res = new List<Instruction>();

		foreach (var data in instructions)
		{
			var splitted = data.Split(' ');
			var direction = splitted[0][0] switch
			{
				'U' => Direction.Up,
				'D' => Direction.Down,
				'L' => Direction.Left,
				'R' => Direction.Right,
				_ => throw new NotImplementedException("wtf"),
			};
			var steps = int.Parse(splitted[1]);
			var rgb = splitted[2][1..^1];

			res.Add(new(direction, steps, rgb));
		}

		return res;
	}

	static List<Instruction> ParseInput2(string[] instructions)
	{
		var res = new List<Instruction>();

		foreach (var data in instructions)
		{
			var splitted = data.Split(' ');
			var rgb = splitted[2][1..^1];

			var direction = rgb[^1] switch
			{
				'0' => Direction.Right,
				'1' => Direction.Down,
				'2' => Direction.Left,
				'3' => Direction.Up,
				_ => throw new NotImplementedException("wtf")
			};

			var steps = Convert.ToInt32(rgb[1..^1], 16);
			res.Add(new(direction, steps, string.Empty));
		}

		return res;
	}

	record struct Instruction(Direction Heading, int Steps, string RGB);

	enum Direction
	{
		Up,
		Down,
		Left,
		Right
	}
}

