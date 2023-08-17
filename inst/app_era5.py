import re
import tkinter as tk
from datetime import datetime
from tkinter import ttk
import pandas as pd
from request_data import make_request
from tkinter import messagebox
from tkinter import PhotoImage
import os

def app(df,csv_path):
  
  variables = []
  
  
  # Function called when the Combobox is open
  def autocomplete():
      current_input = search_var.get().lower()
      names = [name for name in df["Name"] if name.lower().startswith(current_input)]
      search_menu["values"] = names  # We assign the values that fits with the current string
  
  
  def add_variable():
      var = search_var.get()
      search_var.set("")
      if var in df["Name"].values and var not in variables:
          variables.append(var)
          var_selected.insert(0, var)
  
  
  def clear_all():
      var_selected.delete(0, 'end')
      global variables
      variables = []
      for str_v in string_vars:
          str_v.set("")
  
  
  def __is_hour_valid(hours_str):
      hours = hours_str.split(",")
      return all([(re.match(r'^([0-1]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$', x)) for x in hours])
  
  
  def __is_level_valid(levels_str):
      levels = levels_str.split(",")
      levels = [int(x) for x in levels]
      return all([1 <= x <= 137 for x in levels])
  
  
  def __is_date_valid(date_str):
      try:
          datetime.strptime(date_str, '%Y/%m/%d')
          return True
      except ValueError:
          return False
  
  
  def __is_grid_valid(grid):
      return bool(re.match(r'^\d+\.\d+x\d+\.\d+$', grid))
  
  
  def __is_file_valid(file):
      return bool(re.match(r'^.*[.]nc$', file))
  
  
  def __is_coordinates_valid(coord_array):
      coord_array = [int(x) for x in coord_array]
      N, W, S, E = coord_array
      if not (-90 <= N <= 90) or not (-90 <= S <= 90) or not (-180 <= W <= 180) or not (-180 <= E <= 180):
          return False
  
      if N < S or E < W:
          return False
  
      return True
  
  
  def __is_data_valid(data):
      if not __is_level_valid(data[0]):
          messagebox.showerror("SyntaxWarning", "The geo-potential level must be between 1 and 137")
          return False
      elif not __is_hour_valid(data[1]):
          messagebox.showerror("SyntaxWarning", "The hours must be in format HH:MM:SS")
          return False
      elif not __is_grid_valid(data[2]):
          messagebox.showerror("SyntaxWarning", "The grid must be in float format with a x (e.g 1.0x1.0)")
          return False
      elif not __is_file_valid(data[3]):
          messagebox.showerror("SyntaxWarning", "The file name must end with \".nc\"")
          return False
      elif not __is_date_valid(data[4]) or not __is_date_valid(data[5]):
          messagebox.showerror("SyntaxWarning", "The date must be in format yyyy-mm-dd")
          return False
      elif not __is_coordinates_valid(data[6:]):
          messagebox.showerror("SyntaxWarning", "The coordinates must be valid in degrees.")
          return False
      else:
          return True
  
  def submit_all():
      data = [item.get() for item in string_vars][1:]
      if __is_data_valid(data):
          vars = var_selected.get(0, tk.END)
          ids_vars = df[df['Name'].isin(vars)]["id"].values
          ids_vars = ",".join([str(x) for x in ids_vars])
          main.destroy()
          make_request(data[0], data[1], data[2], data[3], data[4], data[5], "/".join(data[6:]), ids_vars)
  
  
  # Create a tkinter window
  main = tk.Tk()
  main.title("User interface")
  
  # Add icon
  photo = PhotoImage(file = csv_path)
  main.iconphoto(True, photo)
  
  # Create label of variables
  var_label = ttk.Label(main, text="Variables")
  var_label.grid(row=1, column=0, padx=5, pady=5, columnspan=2)
  
  # Create select menu for variables
  search_var = tk.StringVar()  # String search
  search_menu = ttk.Combobox(main, textvariable=search_var, state="normal", justify="left", postcommand=autocomplete, )
  search_menu.grid(row=2, column=0, padx=5, pady=5, columnspan=2)
  
  # Create button to add the selected variable
  var_button = ttk.Button(main, command=add_variable, text="Add variable")
  var_button.grid(row=3, column=0, padx=5, pady=5, columnspan=2)
  
  # List that shows the variables selected
  var_selected = tk.Listbox(main, height=5)
  var_selected.grid(row=4, column=0, padx=5, pady=5, rowspan=5, columnspan=2)
  
  # Level selector
  level_label = ttk.Label(main, text="Geo-potential level (e.g. 1,137)")
  level_label.grid(row=1, column=2, padx=5, pady=5, columnspan=2)
  
  level_var = tk.StringVar()
  level_entry = ttk.Entry(main, textvariable=level_var)
  level_entry.grid(row=2, column=2, padx=5, pady=5, columnspan=2)
  
  # Hour selector
  hour_label = ttk.Label(main, text="Hours (e.g. 00:00:00,23:00:00):")
  hour_label.grid(row=3, column=2, padx=5, pady=5, columnspan=2)
  
  hour_var = tk.StringVar()
  hour_entry = ttk.Entry(main, textvariable=hour_var)
  hour_entry.grid(row=4, column=2, padx=5, pady=5, columnspan=2)
  
  # Grid selector
  grid_label = ttk.Label(main, text="Grid (e.g. 1.0x1.0):")
  grid_label.grid(row=5, column=2, padx=5, pady=5, columnspan=2)
  
  grid_var = tk.StringVar()
  grid_entry = ttk.Entry(main, textvariable=grid_var)
  grid_entry.grid(row=6, column=2, padx=5, pady=5, columnspan=2)
  
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
      direction_label.grid(row=0, column=i * 2, padx=5, pady=5)
  
      direction_var = tk.StringVar()
      direction_entry = ttk.Entry(direction_frame, width=5, textvariable=direction_var)
      direction_entry.grid(row=1, column=i * 2, padx=5, pady=5)
  
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
  
  string_vars = [search_var, level_var, hour_var, grid_var, file_var, first_var, last_var] + direction_vars

  main.mainloop()
