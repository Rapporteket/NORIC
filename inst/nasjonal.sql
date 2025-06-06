use noric_nasjonal;

CREATE TABLE andreprosedyrervarnum AS SELECT * FROM noric_unn.andreprosedyrervarnum;
CREATE TABLE angiopcinum AS SELECT * FROM noric_unn.angiopcinum;
CREATE TABLE angiopcivardel1num AS SELECT * FROM noric_unn.angiopcivardel1num;
CREATE TABLE angiopcivardel2num AS SELECT * FROM noric_unn.angiopcivardel2num;
CREATE TABLE annendiagnostikkvarnum AS SELECT * FROM noric_unn.annendiagnostikkvarnum;
CREATE TABLE aortaklaffoppfvarnum AS SELECT * FROM noric_unn.aortaklaffoppfvarnum;
CREATE TABLE aortaklaffvarnum AS SELECT * FROM noric_unn.aortaklaffvarnum;
CREATE TABLE ctangiovarnum AS SELECT * FROM noric_unn.ctangiovarnum;
CREATE TABLE forlopsoversikt AS SELECT * FROM noric_unn.forlopsoversikt;
CREATE TABLE mitralklaffvarnum AS SELECT * FROM noric_unn.mitralklaffvarnum;
CREATE TABLE pasienterstudier AS SELECT * FROM noric_unn.pasienterstudier;
CREATE TABLE segmentstentnum AS SELECT * FROM noric_unn.segmentstentnum;
CREATE TABLE skjemaoversikt AS SELECT * FROM noric_unn.skjemaoversikt;
CREATE TABLE taviprom AS SELECT * FROM noric_unn.taviprom;

--mitralklaffoppfvarnum

INSERT INTO andreprosedyrervarnum (SELECT * FROM noric_bodoe.andreprosedyrervarnum);
INSERT INTO angiopcinum (SELECT * FROM noric_bodoe.angiopcinum);
INSERT INTO angiopcivardel1num (SELECT * FROM noric_bodoe.angiopcivardel1num);
INSERT INTO angiopcivardel2num (SELECT * FROM noric_bodoe.angiopcivardel2num);
INSERT INTO annendiagnostikkvarnum (SELECT * FROM noric_bodoe.annendiagnostikkvarnum);
INSERT INTO aortaklaffoppfvarnum (SELECT * FROM noric_bodoe.aortaklaffoppfvarnum);
INSERT INTO aortaklaffvarnum (SELECT * FROM noric_bodoe.aortaklaffvarnum);
INSERT INTO ctangiovarnum (SELECT * FROM noric_bodoe.ctangiovarnum);
INSERT INTO forlopsoversikt (SELECT * FROM noric_bodoe.forlopsoversikt);
INSERT INTO mitralklaffvarnum (SELECT * FROM noric_bodoe.mitralklaffvarnum);
INSERT INTO pasienterstudier (SELECT * FROM noric_bodoe.pasienterstudier);
INSERT INTO segmentstentnum (SELECT * FROM noric_bodoe.segmentstentnum);
INSERT INTO skjemaoversikt (SELECT * FROM noric_bodoe.skjemaoversikt);
INSERT INTO taviprom (SELECT * FROM noric_bodoe.taviprom);
