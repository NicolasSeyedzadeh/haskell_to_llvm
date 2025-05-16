import matplotlib.pyplot as plt
import numpy as np

means = {
    "Project Compiler":[212.4, 192.3, 302.5, 176.1],
#    "GHC Native no -O2":  [861.4, 854.4, 1333,  868.5],
#    "GHC LLVM no -O2":    [908.2, 951.0, 3427,  937.6],
    "GHC Native":  [918.4,926.1,1064,892.7],
    "GHC LLVM":    [923.9,999.7,1153,969.5],
#    "C no -O3":           [195.7, 182.7, 236,   203.5],
    "C": [200.0,195.6,1926,194.5],
    "OCaML":       [242.6, 249.6, 308.6, 234.1],
}

stds = {
    "Project Compiler":[30.9, 24.1, 34.3, 33.3],
#    "GHC Native no -O2":  [26.7, 98.8, 61.0, 47.2],
#    "GHC LLVM no -O2":    [52.1, 51.9, 44.0, 48.9],
    "GHC Native":  [70.6, 48.9, 74, 41.6],
    "GHC LLVM":    [38.3, 59.4, 57, 63.7],
#    "C no -O3":           [29.8, 33.9, 29.8, 22.4],
    "C": [25.8, 34.9, 43.0, 43.3],
    "OCaML":       [26.8, 31.3, 57, 30.6],
}

samples = ["Simple", "Recursive", "Repeated", "SafeDiv"]
compilers = list(means.keys())
x = np.arange(len(samples))  # the label locations
width = 0.15  # width of each bar

fig, ax = plt.subplots(figsize=(12, 6))

for i, compiler in enumerate(compilers):
    offset = (i - 2) * width  # center the bars
    ax.bar(
        x + offset,
        means[compiler],
        width,
        yerr=stds[compiler],
        label=compiler,
        capsize=5,
        error_kw={'elinewidth': 1, 'ecolor': 'black'}
    )

# Get current y-ticks
y_ticks = plt.yticks()[0]

# Add horizontal lines at each y-tick
for y in y_ticks:
    plt.axhline(y=y, color='lightgray', linestyle='--', linewidth=0.5)
#ax.set_yscale('log')


# Axis formatting
ax.set_ylabel("Compile Time (ms)",fontsize=14)
ax.set_title("Mean Compile Times of Direct Compiler and Alternatives",fontsize=18)
ax.set_xticks(x)
ax.set_xticklabels(samples)
ax.legend()
plt.tight_layout()
plt.savefig("evaluation/results_plot/compile_times.png", dpi=300, bbox_inches='tight')
