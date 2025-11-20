import collections
import csv


def main():
    with open("output/dataset.csv", newline="") as f_in:
        csv_reader = csv.DictReader(f_in)
        counter = collections.Counter(int(x["age"]) for x in csv_reader)

    with open("output/count_by_age.csv", "w", newline="") as f_out:
        csv_writer = csv.writer(f_out)
        csv_writer.writerow(["age", "count"])
        csv_writer.writerows(x for x in sorted(counter.items(), key=lambda x: x[0]))


if __name__ == "__main__":
    main()