%HOW TO RUN: (1) load make sure you are in the correct directory 
% (2) swipl command 
% (3) load the program: [medical_diagnosis].
% (4) run command: start_diagnosis.
% (5) enter symptoms as a list

% CITATIONS:
% (1) how to write in prolog: https://www.educba.com/prolog-write/
% (2) block of code below from chat gpt to help 

% findall(Condition, diagnosis(Condition), Conditions),
%     (Conditions = [] ->
%         write('No diagnosis could be made with the given symptoms.');
%         write('Based on your symptoms, you may have: '), write(Conditions)), nl.

% Declare has_symptom/1 as a dynamic predicate
:- dynamic has_symptom/1.


% Define symptoms associated with each diagnosis
symptom(common_cold, runny_nose).
symptom(common_cold, sore_throat).
symptom(common_cold, cough).
symptom(common_cold, sneezing).
symptom(flu, fever).
symptom(flu, chills).
symptom(flu, body_aches).
symptom(flu, fatigue).
symptom(flu, cough).
symptom(flu, sore_throat).
symptom(allergies, runny_nose).
symptom(allergies, sneezing).
symptom(allergies, itchy_eyes).
symptom(allergies, nasal_congestion).
symptom(strep_throat, sore_throat).
symptom(strep_throat, fever).
symptom(strep_throat, swollen_lymph_nodes).
symptom(strep_throat, headache).
symptom(covid, fever).
symptom(covid, dry_cough).
symptom(covid, shortness_of_breath).
symptom(covid, fatigue).
symptom(covid, loss_of_taste_or_smell).
symptom(migraine, headache).
symptom(migraine, nausea).
symptom(migraine, sensitivity_to_light).
symptom(migraine, sensitivity_to_sound).
symptom(gastroenteritis, nausea).
symptom(gastroenteritis, vomiting).
symptom(gastroenteritis, diarrhea).
symptom(gastroenteritis, stomach_cramps).
symptom(gastroenteritis, fever).
symptom(pneumonia, cough).
symptom(pneumonia, fever).
symptom(pneumonia, shortness_of_breath).
symptom(pneumonia, chest_pain).
symptom(pneumonia, fatigue).
symptom(bronchitis, cough).
symptom(bronchitis, mucus_production).
symptom(bronchitis, fatigue).
symptom(bronchitis, shortness_of_breath).
symptom(bronchitis, chest_discomfort).
symptom(sinusitis, facial_pain).
symptom(sinusitis, nasal_congestion).
symptom(sinusitis, headache).
symptom(sinusitis, loss_of_smell).
symptom(sinusitis, sore_throat).
symptom(asthma, shortness_of_breath).
symptom(asthma, wheezing).
symptom(asthma, chest_tightness).
symptom(asthma, cough).
symptom(toothache, pain).
symptom(toothache, swelling).
symptom(toothache, sensitivity_to_temperature).
symptom(toothache, gum_inflammation).
symptom(toothache, headache).
symptom(heartburn, burning_sensation).
symptom(heartburn, chest_pain).
symptom(heartburn, sour_taste).
symptom(heartburn, difficulty_swallowing).
symptom(heartburn, throat_irritation).
symptom(tension_headache, dull_headache).
symptom(tension_headache, tightness).
symptom(tension_headache, scalp_tenderness).
symptom(tension_headache, neck_pain).
symptom(tension_headache, fatigue).

% Diagnosis rules for each condition based on symptom combinations
diagnosis(common_cold) :- has_symptom(runny_nose), has_symptom(sore_throat), has_symptom(cough), has_symptom(sneezing).
diagnosis(flu) :- has_symptom(fever), has_symptom(chills), has_symptom(body_aches), has_symptom(fatigue), has_symptom(cough), has_symptom(sore_throat).
diagnosis(allergies) :- has_symptom(runny_nose), has_symptom(sneezing), has_symptom(itchy_eyes), has_symptom(nasal_congestion).
diagnosis(strep_throat) :- has_symptom(sore_throat), has_symptom(fever), has_symptom(swollen_lymph_nodes), has_symptom(headache).
diagnosis(covid) :- has_symptom(fever), has_symptom(dry_cough), has_symptom(shortness_of_breath), has_symptom(fatigue), has_symptom(loss_of_taste_or_smell).
diagnosis(migraine) :- has_symptom(headache), has_symptom(nausea), has_symptom(sensitivity_to_light), has_symptom(sensitivity_to_sound).
diagnosis(gastroenteritis) :- has_symptom(nausea), has_symptom(vomiting), has_symptom(diarrhea), has_symptom(stomach_cramps), has_symptom(fever).
diagnosis(pneumonia) :- has_symptom(cough), has_symptom(fever), has_symptom(shortness_of_breath), has_symptom(chest_pain), has_symptom(fatigue).
diagnosis(bronchitis) :- has_symptom(cough), has_symptom(mucus_production), has_symptom(fatigue), has_symptom(shortness_of_breath), has_symptom(chest_discomfort).
diagnosis(sinusitis) :- has_symptom(facial_pain), has_symptom(nasal_congestion), has_symptom(headache), has_symptom(loss_of_smell), has_symptom(sore_throat).
diagnosis(asthma) :- has_symptom(shortness_of_breath), has_symptom(wheezing), has_symptom(chest_tightness), has_symptom(cough).
diagnosis(toothache) :- has_symptom(pain), has_symptom(swelling), has_symptom(sensitivity_to_temperature), has_symptom(gum_inflammation), has_symptom(headache).
diagnosis(heartburn) :- has_symptom(burning_sensation), has_symptom(chest_pain), has_symptom(sour_taste), has_symptom(difficulty_swallowing), has_symptom(throat_irritation).
diagnosis(tension_headache) :- has_symptom(dull_headache), has_symptom(tightness), has_symptom(scalp_tenderness), has_symptom(neck_pain), has_symptom(fatigue).

% Start diagnosis process
start_diagnosis :-
    write('Please enter your symptoms separated by commas (e.g., fever, cough): '), nl,
    read_line_to_string(user_input, Input),      
    split_string(Input, ",", " ", SymptomStrings), 
    maplist(string_to_atom, SymptomStrings, Symptoms), 
    assert_symptoms(Symptoms),                    
    diagnose,
    clear_symptoms.

% Assert each symptom as a fact
assert_symptoms([]).
assert_symptoms([Symptom | Rest]) :-
    assert(has_symptom(Symptom)),
    assert_symptoms(Rest).

% Diagnose based on asserted symptoms
diagnose :-
    findall(Condition, diagnosis(Condition), Conditions),
    (Conditions = [] ->
        write('No diagnosis could be made with the given symptoms.');
        (write('Based on your symptoms, you may have: '), print_conditions(Conditions))
    ), nl.

% Helper predicate to print conditions without brackets
print_conditions([]).
print_conditions([Condition]) :- write(Condition).
print_conditions([Condition | Rest]) :-
    write(Condition), write(', '),
    print_conditions(Rest).

% Clear all asserted symptoms after diagnosis
clear_symptoms :-
    retractall(has_symptom(_)).
