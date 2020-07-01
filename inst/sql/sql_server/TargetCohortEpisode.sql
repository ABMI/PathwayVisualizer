SELECT @cohort_definition_id as cohort_definition_id,* FROM (
SELECT episode_source_concept_Id as concept_id, COUNT(DISTINCT person_id) as cnt
FROM @oncology_database_schema.@episode_table
WHERE person_id in (SELECT subject_id from @result_database_schema.@cohort_table where cohort_definition_id = @cohort_definition_id)
GROUP BY episode_source_concept_Id
ORDER BY cnt desc) as A
