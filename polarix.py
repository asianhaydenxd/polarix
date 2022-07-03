import sys

def main():
    if len(sys.argv) < 2:
        print("Usage: python .\polarix.py <file>")
        return

    file_name = sys.argv[1]

    with open(file_name, 'r') as f:
        f.read()

if __name__ == "__main__": main()
