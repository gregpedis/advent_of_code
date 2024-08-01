using System.Numerics;
using System.Text;

namespace day_x;

internal static class Day21
{
	private const string BROADCASTER = "broadcaster";
	private const int BUTTON_PRESS_COUNT = 1_000;
	public static Queue<WorkItem> ToProcess = new();

	public static BigInteger LowPulses = 0;
	public static BigInteger HighPulses = 0;
	public static BigInteger ButtonPresses = 0;

	public static void Solve()
	{
		var rows = InputReader.ReadRows("Day21.txt");
		var modules = ParseInput(rows);

		PrintDiagram(modules);
		Console.WriteLine(Part1(modules));
		Console.WriteLine(Part2(modules));
	}

	private static BigInteger Part1(Dictionary<string, Module> modules)
	{
		for (int i = 0; i < BUTTON_PRESS_COUNT; i++)
		{
			ToProcess.Enqueue(new("button", BROADCASTER, Pulse.Low));

			while (ToProcess.TryDequeue(out var item))
			{
				if (item.Pulse is Pulse.Low)
				{
					LowPulses++;
				}
				else
				{
					HighPulses++;
				}
				if (modules.TryGetValue(item.Destination, out var destination))
				{
					destination.Receive(item.Pulse, item.Source);
				}
			}
		}

		return LowPulses * HighPulses;
	}

	// This will never end.
	// The diagram is a sequence of 4 distinct sub-graphs.
	// Each sub-graph ends with the last NAND "on", meaning sending a low signal at a different cycle.
	// The cycles are prime numbers, so you just need to multiply them.
	// See the printed message for each NAND.
	private static BigInteger Part2(Dictionary<string, Module> modules)
	{
		foreach (var module in modules.Values)
		{
			module.Reset();
		}

		while (true)
		{
			ToProcess.Enqueue(new("button", BROADCASTER, Pulse.Low));
			ButtonPresses++;

			while (ToProcess.TryDequeue(out var item))
			{
				if (item.Destination == "rx" && item.Pulse is Pulse.Low)
				{
					return ButtonPresses;
				}
				if (modules.TryGetValue(item.Destination, out var destination))
				{
					destination.Receive(item.Pulse, item.Source);
				}
			}
		}
	}

	private static void PrintDiagram(Dictionary<string, Module> modules)
	{
		var sb = new StringBuilder();
		sb.AppendLine("""digraph "Modules" {""");

		var visited = new HashSet<string>();
		var queue = new Queue<Module>();
		queue.Enqueue(modules[BROADCASTER]);

		while (queue.TryDequeue(out var current))
		{
			if (visited.Contains(current.Id))
			{
				continue;
			}
			visited.Add(current.Id);
			var id = current.Id == BROADCASTER
				? BROADCASTER
				: current is FlipFlop
					? current.Id + "_FF"
					: current.Id + "_NAND";

			foreach (var destId in current.Destinations)
			{
				if (modules.TryGetValue(destId, out var dest))
				{
					var dest_Id = dest.Id == BROADCASTER
						? BROADCASTER
						: dest is FlipFlop
							? dest.Id + "_FF"
							: dest.Id + "_NAND";

					sb.AppendLine($"{id} -> {dest_Id}");
					queue.Enqueue(dest);
				}
				else
				{
					sb.AppendLine($"{id} -> {destId}");
				}
			}
		}

		sb.AppendLine("}");

		Console.WriteLine(sb.ToString());
	}

	private static Dictionary<string, Module> ParseInput(string[] rows)
	{
		var wips = rows.Select(ParseRow).ToList();

		var res = new Dictionary<string, Module>();
		foreach (var wip in wips)
		{
			var sources = wips.Where(x => x.Destinations.Contains(wip.Id)).Select(x => x.Id);

			Module module = wip.Type switch
			{
				ModuleType.Broadcaster => new Broadcaster(wip.Id, [.. wip.Destinations]),
				ModuleType.FlipFlop => new FlipFlop(wip.Id, [.. wip.Destinations]),
				ModuleType.Conjuction => new Conjuction(wip.Id, [.. wip.Destinations], [.. sources]),
				_ => throw new NotImplementedException()
			};
			res[module.Id] = module;
		}

		return res;
	}

	private static WorkInProgress ParseRow(string row)
	{
		if (row.StartsWith(BROADCASTER))
		{
			return new(ModuleType.Broadcaster, BROADCASTER, Destinations());
		}
		else if (row.StartsWith('%'))
		{
			var id = row.Substring(1, row.IndexOf('-') - 1).Trim();
			return new(ModuleType.FlipFlop, id, Destinations());
		}
		else
		{
			var id = row.Substring(1, row.IndexOf('-') - 1).Trim();
			return new(ModuleType.Conjuction, id, Destinations());
		}

		List<string> Destinations() =>
			row
				.Substring(row.IndexOf('>') + 1)
				.Split(',')
				.Select(x => x.Trim())
				.ToList();
	}


	public abstract class Module(string Id, string[] Destinations)
	{
		public string Id { get; init; } = Id;
		public string[] Destinations { get; init; } = Destinations;

		public abstract void Receive(Pulse pulse, string from);
		public abstract void Reset();
	}

	public class Broadcaster(string Id, string[] Destinations) : Module(Id, Destinations)
	{
		public override void Receive(Pulse pulse, string from)
		{
			foreach (var module in Destinations)
			{
				ToProcess.Enqueue(new(Id, module, pulse));
			}
		}

		public override void Reset() { }
	}

	public class FlipFlop(string Id, string[] Destinations) : Module(Id, Destinations)
	{
		public bool On = false;

		public override void Receive(Pulse pulse, string from)
		{
			if (pulse is Pulse.Low)
			{
				var newPulse = On ? Pulse.Low : Pulse.High;
				On = !On;
				foreach (var module in Destinations)
				{
					ToProcess.Enqueue(new(Id, module, newPulse));
				}
			}
		}

		public override void Reset() =>
			On = false;
	}

	public class Conjuction : Module
	{
		public bool On = false;
		private readonly string[] destinations;
		public Dictionary<string, Pulse> Memory;

		private bool HasPrinted = false;

		public Conjuction(string id, string[] destinations, string[] sources) : base(id, destinations)
		{
			this.destinations = destinations;
			Memory = sources.ToDictionary(x => x, x => Pulse.Low);
		}

		public override void Receive(Pulse pulse, string from)
		{
			Memory[from] = pulse;
			var newPulse = Memory.All(x => x.Value is Pulse.High)
				? Pulse.Low
				: Pulse.High;

			if (newPulse is Pulse.Low && !HasPrinted)
			{
				HasPrinted = true;
				Console.WriteLine($"{Id}: {ButtonPresses}");
			}

			foreach (var module in destinations)
			{
				ToProcess.Enqueue(new(Id, module, newPulse));
			}
		}

		public override void Reset()
		{
			foreach (var key in Memory.Keys)
			{
				Memory[key] = Pulse.Low;
			}
		}
	}

	public enum Pulse { Low, High }
	public enum ModuleType { Broadcaster, FlipFlop, Conjuction }
	public record WorkItem(string Source, string Destination, Pulse Pulse);

	public record WorkInProgress(ModuleType Type, string Id, List<string> Destinations);
}


