SELECT id, name, description 
  FROM action;
SELECT ship_id, action_id, position 
  FROM action_bar;
SELECT id, name, primary_attack, description 
  FROM firing_arc;
SELECT ship_id, speed, maneuver_id, energy 
  FROM huge_maneuver_dial;
SELECT ship_id, energy, min_primary_range, max_primary_range 
  FROM huge_ship;
SELECT id, name, description 
  FROM maneuver;
SELECT ship_id, speed, maneuver_id, difficulty 
  FROM maneuver_dial;
SELECT id, name, variant, u_name_id, ship_id, pilot_skill, elite, point_cost, text_box 
  FROM pilot;
SELECT id, name, variant, faction, `size`, attack, agility, hull, shield 
  FROM ship;
SELECT ship_id, firing_arc_id 
  FROM ship_firing_arc;
SELECT id, name 
  FROM unique_name;
SELECT id, name, description 
  FROM upgrade;
SELECT ship_id, upgrade_id, position 
  FROM upgrade_bar;

