# --- CLEAR ENVIRONMENT --- #
import os
import pandas as pd
import numpy as np
import openai
from pathlib import Path

# --- Load Libraries --- #
from openai import OpenAIError
from dotenv import load_dotenv

# --- Set OpenAI API Key --- #
openai.api_key = "add_API_key_here"  # Replace with your actual OpenAI API key

# --- Load Existing or Fresh Data --- #
output_file = Path("add_to_path/analyzed_results.xlsx")
input_file = Path("add_to_path/data.xlsx")

if output_file.exists():
    data = pd.read_excel(output_file)
    print("Loaded previously saved analysis.")
else:
    data = pd.read_excel(input_file)

    # --- Classify Treatment and Prepare Response Fields --- #
    def classify(row):
        if pd.notna(row.get("Q236")) and pd.notna(row.get("Q235")):
            return "Short Paper and Video"
        elif pd.notna(row.get("Q238")) and pd.notna(row.get("Q239")):
            return "Video and Short Paper"
        elif pd.notna(row.get("Q233")):
            return "Short Paper"
        elif pd.notna(row.get("Q232")):
            return "Video"
        elif all(pd.isna(row.get(q)) for q in ["Q236", "Q235", "Q233", "Q238", "Q239", "Q232"]):
            return "Control"
        else:
            return "Other"

    def combined_response(row):
        if pd.notna(row.get("Q236")) and pd.notna(row.get("Q235")):
            return f"{row['Q236']} {row['Q235']}"
        elif pd.notna(row.get("Q238")) and pd.notna(row.get("Q239")):
            return f"{row['Q238']} {row['Q239']}"
        elif pd.notna(row.get("Q233")):
            return row["Q233"]
        elif pd.notna(row.get("Q232")):
            return row["Q232"]
        else:
            return np.nan

    data["treatment_group"] = data.apply(classify, axis=1)
    data["combined_response"] = data.apply(combined_response, axis=1)
    data["response_paper"] = data[["Q236", "Q233", "Q238"]].bfill(axis=1).iloc[:, 0]
    data["response_video"] = data[["Q235", "Q232", "Q239"]].bfill(axis=1).iloc[:, 0]

    # Initialize analysis columns
    data["analysis_combined"] = np.nan
    data["analysis_paper"] = np.nan
    data["analysis_video"] = np.nan

    print("Loaded fresh data and initialized analysis columns.")

# --- Define Analysis Function (API call) --- #
def analyze_response(response_text):
    prompt = (
        "You are analyzing a summary written by a survey respondent after reading a paper or watching a documentary about a randomized controlled trial (RCT) "
        "in the Maratane refugee camp in Mozambique. The RCT studied how financial support impacts social cohesion between refugees and host community members.\n\n"
        "Please evaluate the following response based on four criteria:\n"
        "1. **Balance**: Both / Only refugees / Only hosts / Not specified\n"
        "2. **Empathy/Objectivity**: Empathetic toward ___ / Objective / Neutral / Unclear\n"
        "3. **Favorability**: Favorable toward ___ / Neutral / Unclear\n"
        "4. **Shocks Focus**: Cash transfers / Hurricane / Both / Neither / Unclear\n\n"
        f"Response:\n{response_text}"
    )

    try:
        response = openai.ChatCompletion.create(
            model="gpt-4-turbo",
            messages=[{"role": "user", "content": prompt}],
            temperature=0.3,
        )
        return response.choices[0].message.content
    except OpenAIError as e:
        print(f"API response error: {str(e)}")
        return np.nan

# --- Process a Single Row --- #
def process_row(row):
    if pd.isna(row["analysis_combined"]) and pd.notna(row["combined_response"]):
        row["analysis_combined"] = analyze_response(row["combined_response"])
    if pd.isna(row["analysis_paper"]) and pd.notna(row["response_paper"]):
        row["analysis_paper"] = analyze_response(row["response_paper"])
    if pd.isna(row["analysis_video"]) and pd.notna(row["response_video"]):
        row["analysis_video"] = analyze_response(row["response_video"])
    return row

# --- Run Row-by-Row and Save Every 50 Rows --- #
for i in range(len(data)):
    data.iloc[i] = process_row(data.iloc[i])
    if (i + 1) % 50 == 0 or i + 1 == len(data):
        data.to_excel(output_file, index=False)
        print(f"✅ Saved progress at row {i + 1} / {len(data)}")

print(f"🎉 Analysis complete. Final file saved at:\n{output_file}")
