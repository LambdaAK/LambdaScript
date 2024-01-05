import subprocess

# Define a list of inputs
input_files = ["input1.txt", "input2.txt", "input3.txt"]

# Define the command to run your interpreter (replace 'interpreter' with the actual command)
interpreter_command = "./your_interpreter"

# Iterate over input files
for input_file in input_files:
    # Construct the full command
    full_command = f"{interpreter_command} < {input_file}"

    # Run the command and capture the output
    try:
        output = subprocess.check_output(full_command, shell=True, text=True)

        # Store or process the output as needed
        # For example, you might write it to an output file or include it in documentation
        with open(f"{input_file}_output.txt", "w") as output_file:
            output_file.write(output)

        print(f"Successfully processed {input_file}")
    except subprocess.CalledProcessError as e:
        # Handle errors, if any
        print(f"Error processing {input_file}: {e}")
