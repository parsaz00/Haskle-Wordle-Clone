% Citation: How to use plunit (1): https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)
%                             (2): https://stackoverflow.com/questions/33852800/how-to-run-plunit-tests-in-prolog
%                             (3): https://pbrown.me/blog/swi_prolog_unit_testing_env/

%How to run... copy paste these one by one and run:
% (1) swipl
% (2) [medical_diagnosis_test].
% (3) run_tests.
% medical_diagnosis_test.pl

% Load the plunit library
:- use_module(library(plunit)).

% Load the main program
:- consult(medical_diagnosis).

:- begin_tests(medical_diagnosis).

% Test common cold diagnosis
test(common_cold) :-
    start_diagnosis_test([cough, runny_nose], [common_cold]).

% Test flu diagnosis
test(flu) :-
    start_diagnosis_test([fever, cough, sore_throat, body_aches], [flu]).

% Test allergy diagnosis
test(allergy) :-
    start_diagnosis_test([sneezing, itchy_eyes], [allergy]).

% Test ebola diagnosis
test(ebola) :-
    start_diagnosis_test([fever, weakness, diarrhea, vomiting], [ebola]).

% Test no diagnosis
test(no_diagnosis) :-
    start_diagnosis_test([headache], []).

% Test partial symptoms (shouldn't trigger diagnosis)
test(partial_common_cold) :-
    start_diagnosis_test([cough], []).

% Test multiple symptoms but no match
test(multiple_no_match) :-
    start_diagnosis_test([fever, sneezing], []).

% Helper predicate to run diagnosis tests
start_diagnosis_test(Symptoms, ExpectedConditions) :-
    assert_symptoms(Symptoms),
    findall(Condition, diagnosis(Condition), ActualConditions),
    clear_symptoms,
    assertion(ExpectedConditions == ActualConditions).

% Test symptom assertion
test(assert_symptoms) :-
    assert_symptoms([fever, cough, sore_throat, body_aches]),
    has_symptom(fever),
    has_symptom(cough),
    has_symptom(sore_throat),
    has_symptom(body_aches),
    clear_symptoms.

% Test symptom clearing
test(clear_symptoms) :-
    assert_symptoms([fever, cough]),
    clear_symptoms,
    \+ has_symptom(fever),
    \+ has_symptom(cough),
    \+ has_symptom(sore_throat),
    \+ has_symptom(body_aches).

% Test multiple conditions (if symptoms overlap)
test(multiple_conditions) :-
    % Add temporary symptoms for multiple conditions
    assert((symptom(test_condition1, symptom1))),
    assert((symptom(test_condition2, symptom1))),
    assert((diagnosis(test_condition1) :- has_symptom(symptom1))),
    assert((diagnosis(test_condition2) :- has_symptom(symptom1))),
    
    start_diagnosis_test([symptom1], [test_condition1, test_condition2]),
    
    % Clean up temporary symptoms
    retract((symptom(test_condition1, symptom1))),
    retract((symptom(test_condition2, symptom1))),
    retract((diagnosis(test_condition1) :- has_symptom(symptom1))),
    retract((diagnosis(test_condition2) :- has_symptom(symptom1))).

:- end_tests(medical_diagnosis).