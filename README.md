# OPD Data Parser and IDSP Line List Generator for Institutional Use (ESICMCH, Chennai) 📊

An automated, browser-based epidemiological tool designed for the **Department of Community Medicine, ESIC Medical College & Hospital, KK Nagar, Chennai**. 

This application processes raw outpatient department (OPD) data extracts and automatically generates line lists for the Integrated Disease Surveillance Programme (IDSP) Form P and Form S reporting.

## 🔒 Privacy by Design (No Data Leaves Your Device)
This application is built using **Shinylive (WebAssembly)**. It runs entirely client-side within your local web browser. 
* **Zero Server Uploads:** When you upload patient data (`.xls`, `.xlsx`, or `.aspx`), it is processed using your computer's own memory. 
* **Data Security:** Patient names, IP numbers, and clinical details are **never** transmitted over the internet, ensuring strict compliance with institutional data privacy protocols.

## ✨ Features
* **Automated Syndromic Mapping:** Instantly filters daily OPD registries for infectious diseases (ICD-10 Chapter 1: A00-B99) and specific syndromic presentations (e.g., PUO, SARI, ILI, Acute Diarrhoeal Disease).
* **Dual Export Capabilities:** Download the filtered IDSP line list or the full cleaned raw dataset (including Specialisation variables).
* **Interactive Auditing:** Search, sort, and verify flagged cases directly in the browser before downloading the final CSV.
* **Institutional SOP Integration:** Built-in quick-reference guide for standard ICD-10 mappings used by medical officers and data entry operators.

## 📖 How to Use
1. Access the live dashboard here: **[Insert your GitHub Pages Link Here]**
2. Click **Upload Raw Excel File** and select your daily OPD extract (`.xls`, `.xlsx`, or `.aspx`).
3. View the **Epidemiological Summary Metrics** (Total Records, IDSP Cases Flagged, Top Syndrome).
4. Use the tabs to preview either the Filtered IDSP Line List or the Full Cleaned Data.
5. Click the respective blue buttons to download your sanitized `.csv` reports for IDSP portal submission.

## 🛠️ ICD-10 Coding Logic
The app automatically captures all **Chapter 1 (A00-B99)** infectious diseases, plus the following syndromic mappings:
* **Fever (PUO):** R50.9
* **Altered Sensorium:** R40.2 / G04.9
* **Cough (any duration):** R05
* **Jaundice (< 4 wks):** R17
* **Acute Diarrhoeal Disease:** A09
* **ARI / ILI:** J06.9 / J11
* **SARI:** J22 / J18.9
* **Animal Bites:** Dog (W54, W53, W55), Snake (T63.0 / X20)

## 💻 For Developers: Local Updates
To modify the code or update the ICD-10 algorithms, you must recompile the WebAssembly package before pushing to GitHub.
1. Make changes to `app/app.R`.
2. Run the Shinylive export in your R console:
   ```R
   shinylive::export(appdir = "app", destdir = "docs")


#Developed for Institutional use by:-
* Dr. D. Vignesh
* Dr. Nalam Middleton A.
Department of Community Medicine, ESIC Medical College & Hospital, KK Nagar, Chennai.
