# "Prolog: MedExpert - A way to diagnose potential diseases!"

This project utilizes prolog's powers to bring medical diagnosis into the realm of Prolog, creating an interactive rule-based system that is able to identify user medical conditions based on their inputed symptoms. Users will input the symptoms they have and our program will return to them feedback with suggested conditions that may explain their symptoms. What makes this special is how our team utilizes Prolog’s strengths in logical programming and pattern matching to build a system that mimics the behavior of human reasoning, showcasing Prolog's power in handling complex, rule-based problems with ease and precision.

This project is in fulfillment of the [CPSC 312 2024W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website-2024W1/project.html).
# Team Members
Our team is:

+ Daichi Furukawa (student # 51399111)
+ Yuvraj Upadhyay (student # 47253539): theonewhocanread
+ Amar Gill (student # 51913747)
+ Parsa Seyed Zehtab # (student #84226935)
+ Tovar Montana (student # 71746598)
+ We call ourselves: The Prolog MedExperts!

# Acknowledgments
We received valuable support from various resources:
+ We referred to the following site for handling writing and user inputs in prolog: https://www.educba.com/prolog-write/
+ We used https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27) to help us with unit testing in prolog 

# Product Pitch 
In an age where technology plays an ever-increasing role in healthcare, having a system that is intelligent and accessible as a diagnostic tool can bridge the gap between user curiosity and medical advice. MedExpert is a terminal-based medical diagnosis assistant that allows users to input a list of symptoms and recieve potential medical conditions as output. This project is not just a tool, rather, it is a demonstration of how logical reasoning can be used to provide insight into complex problems using Prolog. 

The problem MedExpert solves is both educational and practical: how do we leverage Prolog's unique logical capabilites to simulate a rule-based medical expert system? MedExpert provides an intuitive way for users to explore possible diagnoses based on their symptoms, using a knowledge base and deduction that mimics human diagnostic reasoning. 

## Key Features: 
+ Symptom-to-diagnosis matching based on predefined rules 
+ Intelligent feedback that supports partial matches and offers logical explanataions 
+ Ability to handle multiple potential diagnoses
+ Ability to handle non-concrete cases where there is not a full match from symptoms to disease

By utilizing Prolog's pattern matching and backtracking capabiliteis, MedExpert provides an interactive experience that challenges users to think about their symptoms and receive potential diagnoses they can explore further. A unique advantage our our design is the fact that it offers transparency in decision-making, giving users not just results but the reasoning behind them. 

MedExpert is designed to engage users who are curious about healthcare and tech. It showcases Prolog's logical depth in a practical setting, offering a user experience that is easy to use and powerful in what it offers. 

# Minimal Viable Project (MVP)

We aim to create a functional diagnostic system that allows users to: 
+ Input a list of symptoms through a user friendly GUI
+ Recieve a diagnosis (or multiple) based on the symptoms entered
+ Understand the logic behind each diagnosis through explanatory output
+ Provide users with recommendations for treatment based on their symptoms. 

The MVP will focus on (1) Building a robust knowledge base with a limited set of conditions and symptoms (2) Implementing core inference rules that match symptoms to (a) potential diagnoses and (b) matches symptoms to potential treatments (3) User input handling to ensure ease of interaction and accurate diagnosis output. 

# Leveraging Prolog's Strengths:

The MVP will leverage Prolog's built-in pattern matching to create a straightforward yet powerful diagnosis rule. Prolog's ability to backtrack will allow the program to explore all possible matches, ensuring users recieve comprehensive feedback based on their input. This will enable a natural flow in which each input is tested against existing rules, and then all relevant diagnoses are returned efficiently. 

The MVP will further utilize Prolog's dynamic predicates to handle temporary user data, which will ensure that the system can simulate real-time diagnosis without persistent changes to the knowledge base. 

# Learning New Prolog Features:

We will be extending our knowledge that is grounded in rule definitions and pattern matching by: 

+ Working with dynamic predicates to manage temporary facts 
+ Understanding how to build recursive predicates for user input validation and symptom assertion 
+ Futher testing our program by utilizing features of plunit to comprehensively test our product and ensure smooth user experiences 

By focusing on these elements, our MVP becomes more than just a simple lookup tool; it showcases Prolog's capabilities in handling logical problems while demonstrating our understanding of expert systems in a practicular use case. 


# Future enhancements 
Our MVP will set the stage for further development such as: 
+ Adding severity levels for symptoms
+ Incorporating follow-up questions that will lead to better diagnostic accuracy 
+ Expanding our knowledge base to include more complex conditions 
+ Providing treatment suggestions 
+ Connecting patients with the appropriate healthcare agencies based on their symptoms and suspected diseases. 


# Proof Of Concept
Our POC showcases the essence of what makes expert systems valuable: logical, rule-based reasoning that simulates human diagnostic thinking. This diagnostic logic is the foundation of our project - it accepts user symptoms, applies predefined rules, and returns potential diagnoses. The interactive nature of MedExpert helps users understand the relationship between their symptoms and possibe conditions, providing insight and educational value. 

## Key Focus: 
+ User interaction and input: The system takes user-entered symptoms through a terminal interface, ensuring ease of use and engagement. 
+ Rule-based diagnosis: Each user-provided symptom is matched against a set of predefined rules representing different conditions 
+ Delivering diagnostic feedback: The system outputs potential diagnoses based on the symptoms provided and highlights how many symptoms match each condition 
+ Partial matches and suggestions: If an exact diagnosis cannot be made, the system suggests potential conditions that share overlapping symptoms and prompts users to provide more information for better accuracy. 
