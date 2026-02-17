from sys import stdout

import arcpy
import os
import sys
import time
import datetime

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '../..'))
sys.path.append(parent_dir)
from arcpy_walksheds import ScriptTool, new_gdb, duration, print_rows

arcpy.env.overwriteOutput = True


home_dir = os.getenv('USERPROFILE') + r'\Documents\GitHub\TIP_Demographics\data\27_31'
point_projects = home_dir + r'\gdbs\lines.gdb\line_projects_021126'
gdb = home_dir + r'\gdbs\lines.gdb'

# Join projects to their buffer distances
buffers = arcpy.MakeTableView_management(home_dir + r'\inputs\Project_buffers.csv', 'buffers')

# Split multipart to single part
point_projects_single = arcpy.MultipartToSinglepart_management(point_projects, gdb + r'\lines_singlepart')
proj_layer = arcpy.MakeFeatureLayer_management(point_projects_single)
arcpy.AddJoin_management(proj_layer, "PROJIS", buffers, "ID")

# Check that join worked
# print_rows(proj_layer)

# Create subsets for each buffer distance
print([f.name for f in arcpy.ListFields(proj_layer)])
half_mi_lines = arcpy.MakeFeatureLayer_management(proj_layer, 'half_mi_lines', where_clause="""Project_buffers.csv.Buffer = '1/2-mile'""")
print_rows(half_mi_lines)


start_time = time.time()
# now = datetime.datetime.now()
# date_time_str = now.strftime('%m-%d-%Y_%Hh%Mm%S')
# sys.stdout = open(home_dir + r'\logs\arcpy_network_buffer' + date_time_str + '.txt', 'w')
# sys.stderr = open(home_dir + r'\logs\arcpy_network_buffer' + date_time_str + '_errors.txt', 'w')


# Run script for each buffer distance

# half mile
ScriptTool(
    input_features=half_mi_lines,
    project_id_field='PROJIS',
    network_dataset=os.getenv('USERPROFILE') + r'\Documents\ArcGIS\Projects\TIP_Demographics\RI_ND_copy.gdb\CTPS_RoadInv2018On_DS',
    demo_geometry_feat={
        home_dir + r'\inputs\census\brmpo_blockgroup.shp': ['GEOID', 'bg'],
        home_dir + r'\inputs\census\brmpo_tract.shp': ['GEOID', 'tract']
    },
    cutoffs=0.5,
    output_tables=[
        home_dir + r'\outputs\lines_half_mi_blockgroup_AF.csv',
        home_dir + r'\outputs\lines_half_mi_tract_AF.csv'
    ],
    gdb=new_gdb(home_dir + r'\gdbs', 'lines_half_mi.gdb'),
    network_step=True,
    census_step=True
)

duration(start_time)
sys.stdout.close()