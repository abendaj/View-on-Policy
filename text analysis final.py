import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
from rouge_score import rouge_scorer
from openpyxl import load_workbook  # Import for loading and updating the Excel file

# Load dataset from the 'Mixed' sheet only
data = pd.read_excel('add_to_path/Until 25-03-2025.xlsx', sheet_name='Mixed')

# Updated preprocessing function to ignore empty values in each row
def preprocess_column(group_df):
    return group_df.apply(lambda row: ' '.join([str(cell).strip() for cell in row if pd.notna(cell) and str(cell).strip() != '']), axis=1)

# Cleaned text for analysis
text_analysis_clean = preprocess_column(data[['Q236', 'Q235', 'Q233', 'Q232', 'Q239', 'Q238']])

# Define the reference summary and keywords
reference_summary = "It investigates how financial security influences social cohesion and the socioeconomic integration of refugees..."
reference_keywords = ["financial security", "social cohesion", "refugees", "Mozambique", "integration", "climate shocks", "resilience"]

# Initialize ROUGE scorer
scorer = rouge_scorer.RougeScorer(['rougeL'], use_stemmer=True)

# Compute ROUGE-L score
def compute_rouge(response, reference_summary):
    score = scorer.score(reference_summary, response)
    return score['rougeL'].fmeasure

# Analyze responses function
def analyze_responses(group_responses, weight_cosine=0.2, weight_rouge=0.2, weight_keyword=0.6):
    vectorizer = TfidfVectorizer()
    tfidf_matrix = vectorizer.fit_transform([reference_summary] + list(group_responses))
    cosine_similarities = cosine_similarity(tfidf_matrix[0:1], tfidf_matrix[1:]).flatten()

    rouge_scores = [compute_rouge(response, reference_summary) for response in group_responses]

    def keyword_match(response):
        return sum(1 for keyword in reference_keywords if keyword in response.lower()) / len(reference_keywords)
    keyword_matches = [keyword_match(response) for response in group_responses]

    combined_scores = [
        (weight_cosine * cosine_sim + weight_rouge * rouge + weight_keyword * keyword)
        for cosine_sim, rouge, keyword in zip(cosine_similarities, rouge_scores, keyword_matches)
    ]
    
    return combined_scores

# Calculate individual scores
individual_scores = analyze_responses(text_analysis_clean)

# Add the scores as a new column to the DataFrame
data['text_analysis'] = individual_scores

# Load the existing workbook and update the 'Mixed' sheet only
output_path = 'add_to_path/Until 25-03-2025.xlsx'
with pd.ExcelWriter(output_path, engine='openpyxl', mode='a', if_sheet_exists='replace') as writer:
    data.to_excel(writer, sheet_name='Mixed', index=False)

print(f"Updated file saved at: {output_path}")
