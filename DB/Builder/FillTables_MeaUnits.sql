
do $$ begin perform DropFunction('SetMeaUnit'); end $$;
create or replace function SetMeaUnit(
   _ID_MeaUnit int
  ,_Name varchar (50)
  ,_LongName varchar (500)
  )
  returns void
as $$
begin
  if exists(select * from MeaUnits where ID_MeaUnit = _ID_MeaUnit) then
    update MeaUnits set Name = _Name, LongName = _LongName where ID_MeaUnit = _ID_MeaUnit;
  else
    insert into MeaUnits(ID_MeaUnit, Name, LongName) select _ID_MeaUnit, _Name, _LongName;
  end if;
end;
$$ language plpgsql;

do $$
begin
	perform SetMeaUnit (1, 'В', 'Вольт');
	perform SetMeaUnit (2, 'А', 'Ампер');
	perform SetMeaUnit (3, 'мВ', 'МиллиВольт');
	perform SetMeaUnit (4, 'мА', 'МиллиАмпер');
	perform SetMeaUnit (5, 'м3/ч', 'Кубометр в час');
	perform SetMeaUnit (6, 'м', 'Метр');
	perform SetMeaUnit (7, 'атм', 'Атмосфера');
	perform SetMeaUnit (8, 'Квт-ч', 'Киловатт-час');
	perform SetMeaUnit (9, 'C', 'Градус C');
	perform SetMeaUnit (10, 'ч', 'Час');
	perform SetMeaUnit (11, 'Вт', 'Ватт');
	perform SetMeaUnit (12, 'Об/мин', 'Оборотов в минуту');
	perform SetMeaUnit (13, '%', '%');
	perform SetMeaUnit (14, 'м3', 'Куб. метр');
	perform SetMeaUnit (15, 'м/с', 'метров в секунду');
	perform SetMeaUnit (16, 'Па', 'Паскаль');
	perform SetMeaUnit (17, 'атм', 'Атмосфер');
	perform SetMeaUnit (18, 'бар', 'Бар');
	perform SetMeaUnit (19, 'кгсм', 'Кг/см2');
	perform SetMeaUnit (20, 'МПа', 'Мегапаскаль');
	perform SetMeaUnit (21, 'т/ч', 'Тонна в час');
	perform SetMeaUnit (22, 'т', 'Тонна');	
	perform SetMeaUnit (23, 'Квар-ч', 'Киловар-час');
	perform SetMeaUnit (24, 'Гц', 'Герц');
	perform SetMeaUnit (25, 'КГц', 'Килогерц');
	perform SetMeaUnit (26, 'МГц', 'Мегагерц');
	perform SetMeaUnit (27, 'ГКал', 'Гигакалория');
	perform SetMeaUnit (28, 'Руб', 'Рубль');
	perform SetMeaUnit (29, 'КВт', 'Киловатт');
end $$;