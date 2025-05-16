import matplotlib.pyplot as plt
import numpy as np

means = {
    "Project Compiler":[56.1 , 59, 58.1 , 58.9 ],
#    "GHC Native no -O2":  [1250., 1212., 1266., 1252.],
#    "GHC LLVM"no -O2:    [1256 , 1216., 1268., 1256.],
    "GHC Native":  [1212.,1237,1261,1263],
    "GHC LLVM":    [1262.,1234.,1255.,1253.],
#    "C no -O3":           [57.4 , 145.1, 57.0 , 57.0 ],
    "C": [60.8,61.7,58.3,59.6],
    "OCaML":       [79.9 , 85.8 , 80.4 , 79.7 ],
}

stds = {
    "Project Compiler":[2.8, 2.7, 2.8, 3.5],
#    "GHC Native no -O2":  [33., 17., 16., 21.],
#    "GHC LLVM no -O2":    [28., 11., 23., 23.],
    "GHC Native":  [25.,18.,14.,23.],
    "GHC LLVM":    [25.,22.,33.,16.],
#    "C no -O3":           [6.2, 2.9, 2.1, 2.6],
    "C": [3.5, 4.7, 4. , 4.8],
    "OCaML":       [3.4, 3.6, 3.1, 3.4],
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


#ax.set_yscale('log')
# Get current y-ticks
y_ticks = plt.yticks()[0]

# Add horizontal lines at each y-tick
for y in y_ticks:
    plt.axhline(y=y, color='lightgray', linestyle='--', linewidth=0.5)

# Axis formatting
ax.set_ylabel("Run Time (ms)",fontsize=18)
ax.set_title("Mean Run Times of Outputs Direct Compiler and Alternatives After 100 runs",fontsize=18)
ax.set_xticks(x)
ax.set_xticklabels(samples)
ax.legend()
plt.tight_layout()
plt.savefig("evaluation/results_plot/run_times.png", dpi=300, bbox_inches='tight')
