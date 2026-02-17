# Generate area fractions for polygon projects. Since most (all?) of the projects use municipal boundaries as their
# areas, we could probably just use muni level demographics and tabulate as necessary. But in theory some projects may
# use an irregular boundary, so we will continue with this methodology.

from sys import stdout
import arcpy
import os
import sys
import time
import datetime

parent_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), '../..'))
sys.path.append(parent_dir)
from arcpy_walksheds import new_gdb, duration
from arcpy_walksheds_polygons import polygon_analysis

arcpy.env.overwriteOutput = True


home_dir = os.getenv('USERPROFILE') + r'\Documents\GitHub\TIP_Demographics'
polygon_projects = home_dir + r'\data\27_31\gdbs\polygons.gdb\polygon_projects_cleaned'
test_project = home_dir + r'\data\27_31\gdbs\polygons.gdb\test'
gdb = new_gdb(home_dir + r'\data\27_31\gdbs', 'polygons.gdb')

start_time = time.time()
# now = datetime.datetime.now()
# date_time_str = now.strftime('%m-%d-%Y_%Hh%Mm%S')
# sys.stdout = open(home_dir + r'\logs\arcpy_network_buffer' + date_time_str + '.txt', 'w')
# sys.stderr = open(home_dir + r'\logs\arcpy_network_buffer' + date_time_str + '_errors.txt', 'w')


polygon_analysis(
    input_polygons=polygon_projects,
    # input_polygons=test_project,
    project_id_field='projis',
    network_segments=os.getenv('USERPROFILE') + r'\Documents\ArcGIS\Projects\TIP_Demographics\RI_ND_copy.gdb\CTPS_RoadInv2018On_DS\CTPS_RoadInv2018On_Segments',
    demo_geometry_feat={
        home_dir + r'\data\27_31\inputs\census\brmpo_blockgroup.shp': ['GEOID', 'bg'],
        home_dir + r'\data\27_31\inputs\census\brmpo_tract.shp': ['GEOID', 'tract']
    },
    output_tables=[
        home_dir + r'\data\27_31\outputs\polygon_projects_blockgroup_AF.csv',
        home_dir + r'\data\27_31\outputs\polygon_projects_tract_AF.csv'
    ],
    gdb=gdb
)

duration(start_time)
# sys.stdout.close()