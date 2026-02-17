import arcpy
import os

from arcpy_walksheds import print_rows

arcpy.env.overwriteOutput = True

home_dir = os.getenv('USERPROFILE') + r'\Documents\GitHub\TIP_Demographics'
polygon_projects = home_dir + r'\data\27_31\gdbs\polygons.gdb\polygon_projects'
polygon_projects_cleaned = home_dir + r'\data\27_31\gdbs\polygons.gdb\polygon_projects_cleaned'
print_rows(polygon_projects_cleaned)
test_project = home_dir + r'\data\27_31\gdbs\polygons.gdb\test'
# gdb = new_gdb(home_dir + r'\data\27_31\gdbs', 'polygons.gdb')

# Add/fix service areas for RTA projects
rta_boundaries = home_dir + r'\data\27_31\inputs\RTA_Boundaries.gdb\RTA_Boundaries_2024'
mpo_boundary = home_dir + r'\data\27_31\inputs\mpo_boundary.shp'

# Clip RTA boundaries to MPO boundary
out_features = home_dir + r'\data\27_31\inputs\RTA_Boundaries.gdb\RTA_Boundaries_2024_MPO_clip'
arcpy.Clip_analysis(rta_boundaries, mpo_boundary, out_features)

# Assign correct service area geometry to each project

#RTAs
rta_shapes_dict = {}
with arcpy.da.SearchCursor(out_features, ['RTA', 'SHAPE@']) as rta_shapes:
    for row in rta_shapes:
        print(row)
        rta_shapes_dict[row[0]] = row[1]

print(rta_shapes_dict)

with arcpy.da.UpdateCursor(polygon_projects_cleaned, ['rta', 'SHAPE@']) as cursor:
    for row in cursor:
        try:
            row[1] = rta_shapes_dict[row[0]]
            cursor.updateRow(row)
            print("Filled:", row[0])
        except:
            "No shape for that RTA."


# MBTA core and extended service areas
#
# Source: MBTA Blue Book Open Data Portal
# (had to run repair geometry on it for Dissolve to work)

# CORE
mbta_core = home_dir + r'\data\27_31\inputs\MBTA_core_SA.gdb\Core_Service_Area'

# Need to dissolve munis together - use a fake field
arcpy.AddField_management(mbta_core, 'dissolve_field2', 'TEXT')
arcpy.CalculateField_management(mbta_core, 'dissolve_field2', """'dissolve'""", "PYTHON3")
print_rows(mbta_core)
core_sa_diss = arcpy.Dissolve_management(in_features=mbta_core,
                                    out_feature_class=home_dir + r'\data\27_31\inputs\MBTA_core_SA.gdb\core_sa_dissolve',
                                    dissolve_field='dissolve_field2',
                                    multi_part='MULTI_PART')
print_rows(core_sa_diss)

# EXTENDED
# Note: not clipping to MPO boundary because we just use census geos from the MPO, so the stuff outside will get dropped
mbta_extended = home_dir + r'\data\27_31\inputs\MBTA_Extended_Service_Area\MBTA_Extended_Service_Area.shp'

# Need to dissolve munis together - use a fake field
arcpy.RepairGeometry_management(mbta_extended)
dissolve_field = 'diss_field'
arcpy.AddField_management(mbta_extended, dissolve_field, 'TEXT')
arcpy.CalculateField_management(mbta_extended, dissolve_field, """'dissolve'""", "PYTHON3")
print_rows(mbta_extended)
extended_sa_diss = arcpy.Dissolve_management(in_features=mbta_extended,
                                    out_feature_class=home_dir + r'\data\27_31\inputs\MBTA_core_SA.gdb\core_sa_dissolve',
                                    dissolve_field=dissolve_field,
                                    multi_part='MULTI_PART')
print_rows(extended_sa_diss)


# populate dictionary with service area shapes
mbta_shapes_dict = {}
with arcpy.da.SearchCursor(core_sa_diss, ['SHAPE@']) as shape_cursor:
    for row in shape_cursor:
        print(row)
        mbta_shapes_dict['MBTA Core'] = row[0]

with arcpy.da.SearchCursor(extended_sa_diss, ['SHAPE@']) as shape_cursor:
    for row in shape_cursor:
        print(row)
        mbta_shapes_dict['MBTA Extended'] = row[0]

print(mbta_shapes_dict)

with arcpy.da.UpdateCursor(polygon_projects_cleaned, ['rta', 'SHAPE@']) as cursor:
    for row in cursor:
        try:
            row[1] = mbta_shapes_dict[row[0]]
            cursor.updateRow(row)
            print("Filled:", row[0])
        except:
            print("No shape for that RTA.")