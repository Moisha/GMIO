pg_gm_db_template_from_file Template_ArchRequestStartDate
pg_gm_db_template_str_replace #Appendix# ForDevice
pg_gm_db_template_str_replace #Table# ArchHour
pg_gm_db_template_str_replace #IntrvLen# hour
pg_gm_db_template_str_replace #Identifier# _ID_Device
pg_gm_db_template_str_replace #Condition# ID_Prm in (select ID_Prm from Params where ID_Device = _ID_Device)