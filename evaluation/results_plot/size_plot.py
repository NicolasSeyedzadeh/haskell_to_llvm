import matplotlib.pyplot as plt
import numpy as np
means = {
    "Project Compiler":[15920, 15920, 15920, 15920],
    "GHC Native":  [5678568, 5683632, 5817832, 5679248],
    "GHC LLVM":    [5679168, 5684512, 5864632, 5679248],
    "C":           [15968, 15992, 15968, 15992],
    "OCaML":       [1715360, 1715408, 1715200, 1715312],
}

samples = ["Simple", "Recursive", "Repeated", "SafeDiv"]

compilers = list(means.keys())
x = np.arange(len(samples))
width = 0.15

fig, ax = plt.subplots(figsize=(12, 6))

for i, compiler in enumerate(compilers):
    offset = (i - 2) * width
    ax.bar(
        x + offset,
        means[compiler],
        width,
        label=compiler
    )

ax.set_yscale('log')

y_ticks = plt.yticks()[0]

for y in y_ticks:
    plt.axhline(y=y, color='lightgray', linestyle='--', linewidth=0.5)

# Labeling and formatting
ax.set_ylabel("Log Binary Size (bytes) ",fontsize=14)
ax.set_title("Output Exectuable Sizes for Project and Alternative Compilers",fontsize=18)
ax.set_xticks(x)
ax.set_xticklabels(samples)
ax.legend()
plt.tight_layout()

# Save to file
plt.savefig("evaluation/results_plot/memory_usage_logscale.png", dpi=300, bbox_inches='tight')
