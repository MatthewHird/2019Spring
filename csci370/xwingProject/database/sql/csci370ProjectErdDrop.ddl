ALTER TABLE upgrade_bar DROP FOREIGN KEY FKupgrade_ba385697;
ALTER TABLE upgrade_bar DROP FOREIGN KEY FKupgrade_ba814753;
ALTER TABLE action_bar DROP FOREIGN KEY FKaction_bar707930;
ALTER TABLE action_bar DROP FOREIGN KEY FKaction_bar534866;
ALTER TABLE ship_firing_arc DROP FOREIGN KEY FKship_firin407817;
ALTER TABLE ship_firing_arc DROP FOREIGN KEY FKship_firin858355;
ALTER TABLE huge_maneuver_dial DROP FOREIGN KEY FKhuge_maneu835924;
ALTER TABLE huge_maneuver_dial DROP FOREIGN KEY FKhuge_maneu336777;
ALTER TABLE maneuver_dial DROP FOREIGN KEY FKmaneuver_d5382;
ALTER TABLE maneuver_dial DROP FOREIGN KEY FKmaneuver_d597695;
ALTER TABLE pilot DROP FOREIGN KEY FKpilot831152;
ALTER TABLE pilot DROP FOREIGN KEY FKpilot226174;
ALTER TABLE huge_ship DROP FOREIGN KEY FKhuge_ship156730;
DROP TABLE IF EXISTS action;
DROP TABLE IF EXISTS action_bar;
DROP TABLE IF EXISTS firing_arc;
DROP TABLE IF EXISTS huge_maneuver_dial;
DROP TABLE IF EXISTS huge_ship;
DROP TABLE IF EXISTS maneuver;
DROP TABLE IF EXISTS maneuver_dial;
DROP TABLE IF EXISTS pilot;
DROP TABLE IF EXISTS ship;
DROP TABLE IF EXISTS ship_firing_arc;
DROP TABLE IF EXISTS unique_name;
DROP TABLE IF EXISTS upgrade;
DROP TABLE IF EXISTS upgrade_bar;
