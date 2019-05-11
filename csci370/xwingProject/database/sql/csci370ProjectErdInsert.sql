INSERT INTO action
  (id, 
  name, 
  description) 
VALUES 
  (?, 
  ?, 
  ?);
INSERT INTO action_bar
  (ship_id, 
  action_id, 
  position) 
VALUES 
  (?, 
  ?, 
  ?);
INSERT INTO firing_arc
  (id, 
  name, 
  primary_attack, 
  description) 
VALUES 
  (?, 
  ?, 
  ?, 
  ?);
INSERT INTO huge_maneuver_dial
  (ship_id, 
  speed, 
  maneuver_id, 
  energy) 
VALUES 
  (?, 
  ?, 
  ?, 
  ?);
INSERT INTO huge_ship
  (ship_id, 
  energy, 
  min_primary_range, 
  max_primary_range) 
VALUES 
  (?, 
  ?, 
  ?, 
  ?);
INSERT INTO maneuver
  (id, 
  name, 
  description) 
VALUES 
  (?, 
  ?, 
  ?);
INSERT INTO maneuver_dial
  (ship_id, 
  speed, 
  maneuver_id, 
  difficulty) 
VALUES 
  (?, 
  ?, 
  ?, 
  ?);
INSERT INTO pilot
  (id, 
  name, 
  variant, 
  u_name_id, 
  ship_id, 
  pilot_skill, 
  elite, 
  point_cost, 
  text_box) 
VALUES 
  (?, 
  ?, 
  ?, 
  ?, 
  ?, 
  ?, 
  ?, 
  ?, 
  ?);
INSERT INTO ship
  (id, 
  name, 
  variant, 
  faction, 
  `size`, 
  attack, 
  agility, 
  hull, 
  shield) 
VALUES 
  (?, 
  ?, 
  ?, 
  ?, 
  ?, 
  ?, 
  ?, 
  ?, 
  ?);
INSERT INTO ship_firing_arc
  (ship_id, 
  firing_arc_id) 
VALUES 
  (?, 
  ?);
INSERT INTO unique_name
  (id, 
  name) 
VALUES 
  (?, 
  ?);
INSERT INTO upgrade
  (id, 
  name, 
  description) 
VALUES 
  (?, 
  ?, 
  ?);
INSERT INTO upgrade_bar
  (ship_id, 
  upgrade_id, 
  position) 
VALUES 
  (?, 
  ?, 
  ?);

