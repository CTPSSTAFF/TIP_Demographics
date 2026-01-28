from sys import stdout

import arcpy
import os
import sys
import time
import datetime

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '../..'))
sys.path.append(parent_dir)
from arcpy_walksheds import ScriptTool, new_gdb, duration

arcpy.env.overwriteOutput = True


home_dir = os.getenv('USERPROFILE') + r'\Documents\GitHub\TIP_Demographics'
rescore_projects = home_dir + r'\data\27_31\gdbs\rescore_projects.gdb\rescore_projects'
gdb = new_gdb(home_dir + r'\data\27_31\gdbs', 'rescore_projects.gdb')

start_time = time.time()
# now = datetime.datetime.now()
# date_time_str = now.strftime('%m-%d-%Y_%Hh%Mm%S')
# sys.stdout = open(home_dir + r'\logs\arcpy_network_buffer' + date_time_str + '.txt', 'w')
# sys.stderr = open(home_dir + r'\logs\arcpy_network_buffer' + date_time_str + '_errors.txt', 'w')

ScriptTool(
    input_features=rescore_projects,
    project_id_field='PROJIS',
    network_dataset=os.getenv('USERPROFILE') + r'\Documents\ArcGIS\Projects\TIP_Demographics\RI_ND_copy.gdb\CTPS_RoadInv2018On_DS',
    demo_geometry_feat={
        home_dir + r'\data\27_31\inputs\census\brmpo_blockgroup.shp': ['GEOID', 'bg'],
        home_dir + r'\data\27_31\inputs\census\brmpo_tract.shp': ['GEOID', 'tract']
    },
    cutoffs=0.5, # rescore projects are all 1/2 mile (see "Project buffers_rescore.csv")
    output_tables=[
        home_dir + r'\data\27_31\outputs\rescore_projects_blockgroup_AF.csv',
        home_dir + r'\data\27_31\outputs\rescore_projects_tract_AF.csv'
    ],
    gdb=gdb,
    network_step=True,
    census_step=True
)

duration(start_time)
sys.stdout.close()