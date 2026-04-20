# Masters_Thesis
Quantitative Analysis of Shift in Search Interest of Non-Consensual Sexualised GenAI Content following new AI Model Launches. This repository contains the full research pipeline for my Master of Science in Criminology thesis at the University of Pennsylvania. This looks at search volume trends of deepfake technology and AI-generated adult content.

The primary quantitative component utilizes five years of Google Trends data modeled through a Poisson GLM with a log link to handle normalized Search Volume Index points. This approach allows for the reporting of results as Incidence Rate Ratios, which quantify the proportional growth of search interest following platform shocks. 

Key findings now hosted in this repository include the documentation of a 12.5-fold search demand amplification following the release of "Spicy Mode" and the identification of a "scandal-as-advertising" mechanism where regulatory backlash in 2026 drove Grok-specific interest even as general category demand declined.Complementing the trend analysis, the repository includes a user-level behavioral study based on over 5,600 web-scraped posts from the r/Grok Reddit community.

This section of the research utilised an adversarial intent classifier with a 0.95 F1 score to identify marker categories such as named targeting, coercive intent, and moderation bypass instructions. By linking these community discussions to population-level search signals, the project provides a comprehensive look at how platform permissibility and capability decisions restructure the opportunity for digital harm.

The goal of this model is not causal inference but forward-looking risk prediction to assess how well we can forecast shifts in NCII demand and to simulate how future platform iterations might alter demand trajectories.





