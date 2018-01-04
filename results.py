# cat genetic_results.txt | grep ":" | cut -c1-3,5-8,10-14 | sed 's/:/,/g' > genetics_results.csv

import matplotlib.pyplot as plt
import pandas as pd

def main():
    data_file = "genetics_results.csv"
    data = pd.read_csv(data_file, header=None)
    data.columns = ["thread", "generation", "score"]

    for t in data["thread"].unique():
        t_data = data[data["thread"] == t]
        plt.plot(t_data["generation"], t_data["score"])

    plt.title("Genetic Parallel Algorithm - 64 Cores")
    plt.xlabel("Generation")
    plt.ylabel("Score")
    plt.show()

if __name__ == "__main__":
    main()
