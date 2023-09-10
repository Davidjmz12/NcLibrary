import re
import tkinter as tk
from datetime import datetime
from tkinter import ttk
from request_data import make_request
from tkinter import messagebox
from tkinter import PhotoImage
import pandas as pd


def app(df_var, df_lvl):
    variables = []

    # Function called when the Combobox is open
    def autocomplete_var():
        current_input = search_var.get().lower()
        names = [name for name in df_var["Name"] if name.lower().startswith(current_input)]
        search_menu["values"] = names  # We assign the values that fits with the current string

    def add_variable():
        var = search_var.get()
        search_var.set("")
        if var in df_var["Name"].values and var not in variables:
            variables.append(var)
            var_selected.insert(0, var)

    def clear_all():
        var_selected.delete(0, 'end')
        global variables
        variables = []
        for str_v in string_vars:
            str_v.set("")

    def check_lvl():

        if not __is_unit_valid():
            messagebox.showerror("SelectionError", "You must select a unit for the level.")
        else:
            try:
                closest = ",".join([str(item) for item in __get_closest(lvl_entry.get().split(","))])
                check_label.config(text=closest)
            except ValueError:
                messagebox.showerror("SyntaxError", "The level is not a float value.")

    def __get_closest(arr):
        arr = [float(item) for item in arr]
        numbers = [float(item) for item in df_lvl.iloc[:, int(selected_unit.get())].values]
        return [min(numbers, key=lambda x: abs(x - item)) for item in arr]

    def __is_hour_valid(hours_str):
        hours = hours_str.split(",")
        return all([(re.match(r'^([0-1]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$', x)) for x in hours])

    def __is_unit_valid():
        return selected_unit.get() != ""

    def __is_level_valid(levels_str):
        try:
            [float(item) for item in lvl_entry.get().split(",")]
            return True
        except ValueError:
            return False

    def __is_date_valid(date_str):
        try:
            date = datetime.strptime(date_str, '%Y/%m/%d')
            # Define the start and end dates for the desired range
            start_date = datetime(1960, 1, 1)
            end_date = datetime.today()

            # Check if the date is within the specified range
            return start_date <= date <= end_date
        except ValueError:
            return False

    def __is_grid_valid(grid):
        return bool(re.match(r'^\d+\.\d+x\d+\.\d+$', grid))

    def __is_file_valid(file):
        return bool(re.match(r'^.*[.]nc$', file))

    def __is_coordinates_valid(coord_array):
        try:
            coord_array = [int(x) for x in coord_array]
            N, W, S, E = coord_array
            if not (-90 <= N <= 90) or not (-90 <= S <= 90) or not (-180 <= W <= 180) or not (-180 <= E <= 180):
                return False

            if N < S or E < W:
                return False

            return True
        except ValueError:
            return False

    def __is_data_valid(data):
        if not __is_unit_valid():
            messagebox.showerror("SelectionError", "You must select one level unit.")
        elif not __is_level_valid(data[0]):
            messagebox.showerror("SyntaxError", "The level is not a float value.")
            return False
        elif not __is_hour_valid(data[1]):
            messagebox.showerror("SyntaxError", "The hours must be in format HH:MM:SS")
            return False
        elif not __is_grid_valid(data[2]):
            messagebox.showerror("SyntaxError", "The grid must be in float format with a x (e.g 1.0x1.0)")
            return False
        elif not __is_file_valid(data[3]):
            messagebox.showerror("SyntaxError", "The file name must end with .nc")
            return False
        elif not __is_date_valid(data[4]) or not __is_date_valid(data[5]):
            messagebox.showerror("SyntaxError", "The date must be in format yyyy-mm-dd from January 1960 to present")
            return False
        elif not __is_coordinates_valid(data[6:]):
            messagebox.showerror("SyntaxError", "The coordinates must be valid in degrees.")
            return False
        elif len(var_selected.get(0, tk.END)) == 0:
            messagebox.showerror("SyntaxError", "You must select at least one variable.")
            return False
        else:
            return True

    def submit_all():
        data = [item.get() for item in string_vars][1:]
        if __is_data_valid(data):
            vars = var_selected.get(0, tk.END)
            levels = data[0].split(",")
            closest = __get_closest(levels)
            lvl_index = df_lvl[df_lvl.iloc[:, int(selected_unit.get())].isin(closest)]["n"].values
            lvl_index = ",".join([str(x) for x in lvl_index])
            ids_vars = df_var[df_var['Name'].isin(vars)]["id"].values
            ids_vars = ",".join([str(x) for x in ids_vars])
            main.destroy()
            make_request(lvl_index, data[1], data[2], data[3], data[4], data[5], 
                          "/".join(data[6:]), ids_vars, int(selected_unit.get()), df_lvl)

    # Create a tkinter window
    main = tk.Tk()
    main.resizable(False, False)
    main.title("User interface")

    # Create label of variables
    var_label = ttk.Label(main, text="Variables")
    var_label.grid(row=1, column=0, padx=5, pady=5, columnspan=2)

    # Create select menu for variables
    search_var = tk.StringVar()  # String search
    search_menu = ttk.Combobox(main, textvariable=search_var, state="normal", justify="left",
                               postcommand=autocomplete_var)
    search_menu.grid(row=2, column=0, padx=5, pady=5, columnspan=2)

    # Create button to add the selected variable
    var_button = ttk.Button(main, command=add_variable, text="Add variable")
    var_button.grid(row=3, column=0, padx=5, pady=5, columnspan=2)

    # List that shows the variables selected
    var_selected = tk.Listbox(main, height=5)
    var_selected.grid(row=4, column=0, padx=5, pady=5, rowspan=5, columnspan=2)

    # Hour selector
    hour_label = ttk.Label(main, text="Hours (e.g. 00:00:00,23:00:00):")
    hour_label.grid(row=1, column=2, padx=5, pady=5, columnspan=2)

    hour_var = tk.StringVar()
    hour_entry = ttk.Entry(main, textvariable=hour_var)
    hour_entry.grid(row=2, column=2, padx=5, pady=5, columnspan=2)

    # Grid selector
    grid_label = ttk.Label(main, text="Grid (e.g. 1.0x1.0):")
    grid_label.grid(row=3, column=2, padx=5, pady=5, columnspan=2)

    grid_var = tk.StringVar()
    grid_entry = ttk.Entry(main, textvariable=grid_var)
    grid_entry.grid(row=4, column=2, padx=5, pady=5, columnspan=2)

    # First date selector
    first_label = ttk.Label(main, text="First date (yyyy/mm/dd):")
    first_label.grid(row=1, column=4, padx=5, pady=5, columnspan=4)

    first_var = tk.StringVar()
    first_entry = ttk.Entry(main, textvariable=first_var)
    first_entry.grid(row=2, column=4, padx=5, pady=5, columnspan=4)

    # Last date selector
    last_label = ttk.Label(main, text="Last date (yyyy/mm/dd):")
    last_label.grid(row=3, column=4, padx=5, pady=5, columnspan=4)

    last_var = tk.StringVar()
    last_entry = ttk.Entry(main, textvariable=last_var)
    last_entry.grid(row=4, column=4, padx=5, pady=5, columnspan=4)

    # Create an entry for each direction
    direction_labels = ["North", "West", "South", "East"]
    direction_entries = []
    direction_vars = []

    # Create a frame to hold the direction entries
    direction_frame = ttk.Frame(main)
    direction_frame.grid(row=5, column=4, columnspan=4, padx=5, pady=5, rowspan=2)
    for i, direction in enumerate(direction_labels):
        direction_label = ttk.Label(direction_frame, text=direction)
        direction_label.grid(row=0, column=i, padx=5, pady=5)

        direction_var = tk.StringVar()
        direction_entry = ttk.Entry(direction_frame, width=5, textvariable=direction_var)
        direction_entry.grid(row=1, column=i, padx=5, pady=5)

        direction_vars.append(direction_var)
        direction_entries.append(direction_entry)

    # First date selector
    file_label = ttk.Label(main, text="File name:")
    file_label.grid(row=9, column=0, padx=5, pady=5)

    file_var = tk.StringVar()
    file_entry = ttk.Entry(main, textvariable=file_var)
    file_entry.grid(row=9, column=1, padx=5, pady=5, sticky="W")

    # Create button to add the selected variable
    submit_button = ttk.Button(main, command=submit_all, text="Submit all")
    submit_button.grid(row=9, column=2, padx=5, pady=5)

    # Create button to add the selected variable
    submit_button = ttk.Button(main, command=clear_all, text="Clear all")
    submit_button.grid(row=9, column=3, padx=5, pady=5)

    # Select level
    selected_unit = tk.StringVar()
    units = (("GeoAltitude[m]", 5), ("Half-level[hPa]", 3), ("Temperature[K]", 7))

    # Label
    lev_label = ttk.Label(main, text="Level (10,100):")
    lev_label.grid(row=1, column=8, padx=5, pady=5)

    for i in range(0, len(units)):
        r = ttk.Radiobutton(
            main,
            text=units[i][0],
            value=units[i][1],
            variable=selected_unit
        )
        r.grid(row=2 + i, column=8)

    # Create an entry for the level
    lvl_var = tk.StringVar()  # String search
    lvl_entry = ttk.Entry(main, textvariable=lvl_var)
    lvl_entry.grid(row=5, column=8, padx=5, pady=5)

    # Create button to check variables
    submit_button = ttk.Button(main, command=check_lvl, text="Check")
    submit_button.grid(row=6, column=8, padx=5, pady=5)

    # Label
    check_label = ttk.Label(main, text="")
    check_label.grid(row=7, column=8, padx=5, pady=5)

    def debug():
        search_var.set("Temperature")
        hour_var.set("00:00:00")
        grid_var.set("1.0x1.0")
        file_var.set("a.nc")
        first_var.set("2000/01/01")
        last_var.set("2000/01/01")
        for dir in direction_vars:
            dir.set("1")

    debug()
    string_vars = [search_var, lvl_var, hour_var, grid_var, file_var, first_var, last_var] + direction_vars

    main.mainloop()

a = pd.read_csv("var_names.csv")
b = pd.read_csv("geo_names.csv")

app(a,b)