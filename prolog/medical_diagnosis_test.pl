:- begin_tests(medical_diagnosis).
:- [medical_diagnosis].

% Test case 1: Common cold diagnosis
test(common_cold) :-
    retractall(has_symptom(_)),
    assert(has_symptom(runny_nose)),
    assert(has_symptom(sore_throat)),
    assert(has_symptom(cough)),
    assert(has_symptom(sneezing)),
    findall(Condition, diagnosis(Condition), Conditions),
    Conditions == [common_cold].

% Test case 2: Flu diagnosis
test(flu) :-
    retractall(has_symptom(_)),
    assert(has_symptom(fever)),
    assert(has_symptom(chills)),
    assert(has_symptom(body_aches)),
    assert(has_symptom(fatigue)),
    assert(has_symptom(cough)),
    assert(has_symptom(sore_throat)),
    findall(Condition, diagnosis(Condition), Conditions),
    Conditions == [flu].

% Test case 3: Allergies diagnosis
test(allergies) :-
    retractall(has_symptom(_)),
    assert(has_symptom(runny_nose)),
    assert(has_symptom(sneezing)),
    assert(has_symptom(itchy_eyes)),
    assert(has_symptom(nasal_congestion)),
    findall(Condition, diagnosis(Condition), Conditions),
    Conditions == [allergies].

% Test case 4: COVID-19 diagnosis
test(covid) :-
    retractall(has_symptom(_)),
    assert(has_symptom(fever)),
    assert(has_symptom(dry_cough)),
    assert(has_symptom(shortness_of_breath)),
    assert(has_symptom(fatigue)),
    assert(has_symptom(loss_of_taste_or_smell)),
    findall(Condition, diagnosis(Condition), Conditions),
    Conditions == [covid].

% Test case 5: No diagnosis found
test(no_diagnosis) :-
    retractall(has_symptom(_)),
    assert(has_symptom(unusual_symptom)),
    findall(Condition, diagnosis(Condition), Conditions),
    Conditions == [].

% Test case 6: Multiple matching conditions (e.g., cough and fever could match flu, pneumonia)
test(multiple_matches) :-
    retractall(has_symptom(_)),
    assert(has_symptom(cough)),
    assert(has_symptom(fever)),
    assert(has_symptom(fatigue)),
    assert(has_symptom(chills)),  % Symptom for flu
    assert(has_symptom(shortness_of_breath)),  % Symptom for pneumonia
    findall(S, has_symptom(S), AssertedSymptoms),
    write('Asserted symptoms: '), write(AssertedSymptoms), nl,
    findall(Condition, diagnosis(Condition), Conditions),
    write('Conditions found: '), write(Conditions), nl,
    member(flu, Conditions), !,
    member(pneumonia, Conditions).

% Test case 7: Diagnosis with partial symptoms
test(partial_symptoms) :-
    retractall(has_symptom(_)),
    assert(has_symptom(sneezing)),
    assert(has_symptom(itchy_eyes)),
    assert(has_symptom(runny_nose)),
    assert(has_symptom(nasal_congestion)),
    assert(has_symptom(sore_throat)),
    assert(has_symptom(cough)),
    findall(S, has_symptom(S), AssertedSymptoms),
    write('Asserted symptoms: '), write(AssertedSymptoms), nl,
    findall(Condition, diagnosis(Condition), Conditions),
    write('Conditions found: '), write(Conditions), nl,
    member(common_cold, Conditions), !,
    member(allergies, Conditions).

:- end_tests(medical_diagnosis).
