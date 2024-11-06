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
symptom(common_cold, cough).
symptom(common_cold, runny_nose).
symptom(flu, fever).
symptom(flu, cough).
symptom(flu, sore_throat).
symptom(flu, body_aches).
symptom(allergy, sneezing).
symptom(allergy, itchy_eyes).

% Basic diagnosis rules based on symptom combinations
diagnosis(common_cold) :- has_symptom(cough), has_symptom(runny_nose).
diagnosis(flu) :- has_symptom(fever), has_symptom(cough), has_symptom(sore_throat), has_symptom(body_aches).
diagnosis(allergy) :- has_symptom(sneezing), has_symptom(itchy_eyes).


% Start diagnosis process
start_diagnosis :-
    write('Please enter your symptoms as a list (e.g., [fever, cough]): '), nl,
    read(Symptoms),
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
        write('Based on your symptoms, you may have: '), write(Conditions)), nl.

% Clear all asserted symptoms after diagnosis
clear_symptoms :-
    retractall(has_symptom(_)).
