# Arcpy Walksheds
# --
# -- Written by Seth Strumwasser
# -- Date Created: 10/14/22
# -- Date Updated: 9/30/25
#
# Duplicated from needs-assessment-2025 repo on 1/20/26.

'''
This script calculates area fractions for census polygons within a certain distance of input features. The area
fractions are used to calculate the population of equity groups from census data. The script relies on ArcGIS Network Analyst
Tools, so an extension license is required.
- Inputs
    1. An ESRI feature dataset containing a network dataset and a segments feature class derived from the MassDOT Road Inventory
    2. Input data (points or lines) around which to calculate walksheds
    3. Cenus tracts or block groups
- Outputs
    1. A table with GEOIDs and corresponding area fractions - the percent of the GEOID's roadways that lie within the walksheds
        of the input data.

* Description updated 1/20/26
'''


import sys
import arcpy
import os
import time

def new_gdb(dir, name):
    path = os.path.join(dir, name)
    if arcpy.Exists(path):
        return path
    else:
        arcpy.CreateFileGDB_management(dir, name)
        return path


def duration(start_time, print_time=True):
    """
    Get the amount of time passed since start_time.

    :param start_time: time() object
    :param print_time: bool
    :return: string
    """
    duration = time.time() - start_time
    if duration / 60 < 1:
        duration_string = "--- Time: " + str(round(duration % 60)) + " seconds ---"

    elif duration / 60 < 60:
        duration_string = "--- Time: " + str(round(duration / 60)) + " minutes " + str(round(duration % 60)) + " seconds ---"

    else:
        duration_string = "--- Time: " + str(round(duration / 3600)) + " hour(s) " + str(round(duration % 60)) + " minutes " + \
                          str(round(duration % 60)) + " seconds ---"
    if print_time:
        print(duration_string)
    return duration_string


def main(input_points, project_id_field, solver_object, network_dataset,
               demo_geometry_feat, output_tables, gdb,
         output_to_map=False,
         network_step=True,
         census_step=True,
         input_network_poly_buf='',
         input_poly_buf=''):

    """
    :param input_points: Path to input points
    :param project_id_field: Field containing ID (str) for groups of locations (i.e. a TIP project)
    :param network_dataset: An ESRI dataset containing a network dataset and segments feature class
    :param demo_geometry_feat: Dictionary with structure: {[path to census features (tracts or block groups)]: list["geoid field name", "census geom (tract or bg)"]}
    :param output_table: Path of output table to create
    :param gdb: Geodatabase where intermediate values will be saved
    :param cutoffs: Floating point number representing distance in miles
    :param network_step: Boolean indicating whether to run network buffer step
    :param census_step: Boolean indicating whether to run census identity step
    :param input_network_poly_buf: Location to save this dataset - default is in GDB
    :param input_poly_buf: Location to save this dataset - default is in GDB
    :return:
    """
    arcpy.env.workspace = gdb

    # if these params are left empty, set to default
    if input_network_poly_buf == '':
        input_network_poly_buf = gdb + r'\input_network_poly_buf'
    if input_poly_buf == '':
        input_poly_buf = gdb + r'\input_poly_buf'

    project_id_field_blank = False
    if project_id_field == '':
        arcpy.AddMessage('Assigning project ID field...')
        project_id_field = 'ID_field'
        project_id_field_blank = True

    if network_step:

        # If all buffers should be dissolved together:
        if project_id_field_blank:
            # Add project ID field to points so that output line of service area have the right names (not just 'Location ###')
            arcpy.AddMessage(f"Adding project ID field ({project_id_field}) to input points...")
            arcpy.AddField_management(input_points, field_name=project_id_field, field_type='TEXT')
            arcpy.CalculateField_management(in_table=input_points,
                                            field=project_id_field,
                                            expression=f"'{project_id_field}'",
                                            expression_type='PYTHON3')

        # 4a: Add project vertices as locations to Service Area Analysis Layer
        # Note: projects with no ID with lead to a series of vertices named "Location 1", "Location 2", and so on.
        # This creates many output features because they do not dissolve.
        arcpy.AddMessage("4. Adding project vertices as Locations...")

        # Map the project ID field to the "Name" property of the Facilities layer
        print(project_id_field)
        arcpy.na.AddLocations(in_network_analysis_layer='project_service_area',
                             sub_layer='Facilities',
                             in_table=input_points,
                             field_mappings=f"Name {project_id_field} #")

        # 5a: Run Service Area Analysis (Solve)
        arcpy.AddMessage("5. Solving Service Area Layer...")
        arcpy.na.Solve('project_service_area')


        # 6: Export line features from service area analysis
        arcpy.AddMessage("6. Exporting features and making feature layers...")
        arcpy.CopyFeatures_management('project_service_area\Lines', gdb + r'\input_buf')
        arcpy.CopyFeatures_management('project_service_area\Facilities', 'SA_Facilities')
        input_buf_lyr = 'input_buf_lyr'
        arcpy.MakeFeatureLayer_management(gdb + r'\input_buf', input_buf_lyr)

        # 7: Join Facilities layer to input_buf on project_ID
        arcpy.AddMessage("7. Joining Facilities to input_buf...")
        validation = arcpy.ValidateJoin_management(input_buf_lyr, 'FacilityID', gdb + r'\SA_Facilities', 'ObjectID')
        arcpy.AddMessage('Validating Join...')
        arcpy.AddMessage(validation)
        arcpy.AddJoin_management(input_buf_lyr, 'FacilityID', gdb + r'\SA_Facilities', 'ObjectID')

        # 8: Copy features to get rid of qualified field names
        arcpy.env.qualifiedFieldNames = False
        arcpy.AddMessage("8. Exporting features...")
        arcpy.CopyFeatures_management(input_buf_lyr, 'input_buf_joined')


        # 9: Add field for project_ID
        arcpy.AddMessage("9. Adding project ID field...")
        arcpy.AddField_management('input_buf_joined', project_id_field, 'TEXT')


        # 10: Calculate project_ID field from joined Facilities layer 'Name' field
        arcpy.AddMessage("10. Calculating project_ID_field...")
        arcpy.CalculateField_management(gdb + r'\input_buf_joined', project_id_field, "!Name!", "PYTHON3")


        # 11: Dissolve input_buf_joined on project_ID
        arcpy.AddMessage("11. Dissolving on project_ID_field...")
        arcpy.Dissolve_management(gdb + r"\input_buf_joined", gdb + r"\input_buf_diss", project_id_field)


        # 11b. Create polygon buffer around project buffer lines
        arcpy.AddMessage("11b. Creating network buffer polygon buffer...")
        arcpy.Buffer_analysis(in_features=gdb + r'\input_buf_diss',
                              out_feature_class=input_poly_buf,
                              buffer_distance_or_field="5 Meters",
                              dissolve_option="LIST",
                              dissolve_field=project_id_field)




    if census_step:
        # for each feature class in the demo_geometry_feat parameter
        i = 1
        for dataset in demo_geometry_feat:
            geom = dataset
            geoid_field = demo_geometry_feat[geom][0]
            geom_str = demo_geometry_feat[geom][1]

            arcpy.AddMessage("Processing " + geom_str + "/" + str(len(demo_geometry_feat)) + ": " + geom)

            # 13a: Identity using network buffer polygon buffer features
            arcpy.AddMessage("13a. Identity on buffer...")

            arcpy.Identity_analysis(in_features=input_poly_buf,
                                    identity_features=geom,
                                    out_feature_class=gdb + r'\input_poly_buf_iden_' + geom_str,
                                    relationship='NO_RELATIONSHIPS')


            # 13b. Dissolve on project_id_field and geoid to eliminate sliver polygons
            arcpy.AddMessage("13b. Dissolve buffer network...")

            print("project_id_field:", project_id_field)
            print("geoid_field:", geoid_field)
            input_poly_buf_iden_diss = arcpy.Dissolve_management(in_features=gdb + r'\input_poly_buf_iden_' + geom_str,
                                      out_feature_class=gdb + r'\input_poly_buf_iden_diss_' + geom_str,
                                      dissolve_field=[project_id_field, geoid_field])


            # 12: Get all the full network lines in the GEOIDs that the input buffer intersects
            # 12a: Get list of GEIODs
            arcpy.AddMessage("12a. Getting list of GEOIDs")
            geoid_lyr = arcpy.MakeFeatureLayer_management(dataset, 'dataset')

            arcpy.AddJoin_management(
                in_layer_or_view=geoid_lyr,
                in_field=geoid_field,
                join_table=input_poly_buf_iden_diss,
                join_field=geoid_field,
                join_type="KEEP_COMMON"
            )
            arcpy.CopyFeatures_management(geoid_lyr, gdb + r'\join_GEOID_test')

            # Get network segments for the GEOIDs intersected by the input buffer

            # This query is annoying for some reason and reveals that the network this script uses is quite poor. For
            # some reason you have to explicitly include null values - this is important because many walkways and
            # minor roadways are marked null for facility type.
            where = """(facility NOT IN (8, 9, 11, 7) Or facility IS NULL) 
                        And (control = 0 Or control IS NULL) 
                        And (f_f_class <> 1 Or f_f_class IS NULL)"""
            segs = arcpy.MakeFeatureLayer_management(network_dataset + r'\CTPS_RoadInv2018On_Segments',
                                                     where_clause=where)
            segs_clipped = arcpy.Clip_analysis(
                in_features=segs,
                clip_features=geoid_lyr,
                out_feature_class=gdb + r'\network_segs_clipped'
            )

            # 12c. Create a polygon buffer around the network segments
            arcpy.AddMessage("12c. Creating polygon buffer around full network subset...")

            network_segs_buf = arcpy.Buffer_analysis(in_features=segs_clipped,
                                  out_feature_class=gdb + r'\network_segs_buf',
                                  buffer_distance_or_field="5 Meters",
                                  dissolve_option="ALL")

            # 13c: Identity on entire network polygon buffer with census geometry
            arcpy.AddMessage("13. Identity on full network...")

            network_segs_buf_iden = arcpy.Identity_analysis(in_features=network_segs_buf,
                                    identity_features=geom,
                                    out_feature_class=gdb + r'\network_segs_buf_iden_' + geom_str,
                                    relationship='NO_RELATIONSHIPS')


            # 14. Export tables of both layers
            arcpy.AddMessage("14. Exporting tables...")

            network_segs_buf_iden_table = arcpy.ExportTable_conversion(in_table=network_segs_buf_iden,
                                                                       out_table=gdb + r'\network_segs_buf_iden_table_' + geom_str)

            input_poly_buf_iden_table = arcpy.ExportTable_conversion(in_table=gdb + r'\input_poly_buf_iden_' + geom_str,
                                                                     out_table=gdb + r'\input_poly_buf_iden_table_' + geom_str)

            # 15: Join tables
            arcpy.AddMessage("15. Joining tables...")
            input_poly_buf_iden_table_view = arcpy.MakeTableView_management(in_table=input_poly_buf_iden_table,
                                                                            out_view='input_poly_buf_iden_table_view_' + geom_str)
            arcpy.AddJoin_management(in_layer_or_view=input_poly_buf_iden_table_view,
                                     in_field=geoid_field,
                                     join_table=network_segs_buf_iden_table,
                                     join_field=geoid_field)

            # 15b. Export for field name sanity
            arcpy.AddMessage('15b. Exporting joined table...')

            arcpy.env.qualifiedFieldNames = False
            input_poly_buf_iden_joined = arcpy.ExportTable_conversion(in_table=input_poly_buf_iden_table_view,
                                         out_table=gdb + r'\input_poly_buf_iden_joined_' + geom_str)

            # 16: Add area_fraction field
            arcpy.AddMessage("16. Adding area_fraction field...")

            arcpy.AddField_management(in_table=input_poly_buf_iden_joined,
                                      field_name='area_fraction',
                                      field_type='DOUBLE')

            # 17: Calculate area_fraction field
            arcpy.AddMessage("17. Calculating area_fraction field...")

            buffer_area_field = "!Shape_Area!"
            network_area_field = "!shape_Area_1!"

            arcpy.CalculateField_management(in_table=input_poly_buf_iden_joined,
                                            field='area_fraction',
                                            expression=buffer_area_field + r'/' + network_area_field,
                                            expression_type='PYTHON3')

            if project_id_field_blank:
                arcpy.CalculateField_management(input_poly_buf_iden_joined,
                                                project_id_field, "'ID_field'", 'PYTHON3')

            # 18. Export table to csv - fancy field mapping to make the output table clean
            arcpy.AddMessage("18. Exporting to output table...")

            arcpy.env.qualifiedFieldNames = False

            fms = arcpy.FieldMappings()

            # FieldMap objects
            proj_id_fm = arcpy.FieldMap()
            geoid = arcpy.FieldMap()
            area_fraction = arcpy.FieldMap()

            # add input fields
            proj_id_fm.addInputField(input_poly_buf_iden_joined, project_id_field)
            geoid.addInputField(input_poly_buf_iden_joined, geoid_field)
            area_fraction.addInputField(input_poly_buf_iden_joined, 'area_fraction')

            # set output fields
            proj_id_name = proj_id_fm.outputField
            proj_id_name.name = project_id_field
            proj_id_fm.outputField = proj_id_name

            geoid_name = geoid.outputField
            geoid_name.name = geoid_field
            geoid.outputField = geoid_name

            area_fraction_name = area_fraction.outputField
            area_fraction_name.name = 'area_fraction'
            area_fraction.outputField = area_fraction_name

            # add FieldMaps to FieldMapping object
            fms.addFieldMap(proj_id_fm)
            fms.addFieldMap(geoid)
            fms.addFieldMap(area_fraction)

            # change name of output table to correct item in parameter list
            try:
                output_table = output_tables[i - 1]
            except IndexError:  # if no table name provided
                output_table = 'out_table_' + geom_str

            arcpy.ExportTable_conversion(in_table=input_poly_buf_iden_joined,
                                         out_table=output_table,
                                         field_mapping=fms)

            # save backup in FGDB
            arcpy.ExportTable_conversion(in_table=input_poly_buf_iden_joined,
                                         out_table=gdb + r'\backup_' + output_table.split("\\")[-1].strip(".csv"),
                                         field_mapping=fms)

            # 19. Create joined output for debugging - this is the shapes of the network buffer joined to the area fraction table

            arcpy.AddMessage('Creating joined output...')
            output_joined_lyr = arcpy.MakeFeatureLayer_management(input_poly_buf_iden_diss, 'output_joined_lyr')
            arcpy.AddJoin_management(in_layer_or_view=output_joined_lyr,
                                     in_field=geoid_field,
                                     join_table=output_table,
                                     join_field=geoid_field,
                                     join_type='KEEP_ALL')

            arcpy.env.qualifiedFieldNames = False

            # Create a field mappings object
            fms2 = arcpy.FieldMappings()

            # FieldMap objects
            proj_id_fm = arcpy.FieldMap()
            geoid = arcpy.FieldMap()
            area_fraction = arcpy.FieldMap()

            # add input fields
            proj_id_fm.addInputField(input_poly_buf_iden_joined, project_id_field)
            geoid.addInputField(input_poly_buf_iden_joined, geoid_field)
            area_fraction.addInputField(input_poly_buf_iden_joined, 'area_fraction')

            # set output fields
            proj_id_name = proj_id_fm.outputField
            proj_id_name.name = project_id_field
            proj_id_fm.outputField = proj_id_name

            geoid_name = geoid.outputField
            geoid_name.name = geoid_field
            geoid.outputField = geoid_name

            area_fraction_name = area_fraction.outputField
            area_fraction_name.name = 'area_fraction'
            area_fraction.outputField = area_fraction_name

            # add FieldMaps to FieldMappings object
            fms2.addFieldMap(proj_id_fm)
            fms2.addFieldMap(geoid)
            fms2.addFieldMap(area_fraction)

            arcpy.ExportFeatures_conversion(in_features=output_joined_lyr,
                                          out_features=gdb + r'\output_joined_' + geom_str,
                                            field_mapping=fms2)

            i += 1

            # end of loop



def ScriptTool(input_features, gdb, project_id_field, cutoffs, network_dataset,
               demo_geometry_feat, output_tables, output_to_map='false', network_step=True, census_step=True,
               input_poly_buf='', input_network_poly_buf=''):

    # gdb = arcpy.env.workspace # using parameter now

    # check input geometry
    desc_input = arcpy.Describe(input_features)
    input_type = desc_input.shapeType
    arcpy.AddMessage('input_type: ' + input_type)

    if input_type == 'Polygon':

        # polygon_analysis(input_features, project_id_field, network_dataset, network_segments,
        #              demo_geometry_feat, output_tables, output_to_map)
        print("Can't run polygons.")

    elif input_type in ['Point', 'Polyline']:
        if network_step:
            # Step 1: Make Service Area Analysis Layer
            arcpy.AddMessage("1. Making Service Area Layer...")

            solver_object = arcpy.na.MakeServiceAreaAnalysisLayer(network_data_source=network_dataset + r'\CTPS_RI2018On_ND',
                                                  layer_name='project_service_area',
                                                  travel_mode='Pedestrian',
                                                  cutoffs=cutoffs,
                                                  output_type='LINES',
                                                  geometry_at_overlaps='OVERLAP',
                                                  accumulate_attributes='length_miles')

        if input_type == 'Point':
            # make a copy of the input to avoid editing original data
            input_points = arcpy.ExportFeatures_conversion(input_features, gdb + r'\input_points_copy')

        elif input_type == 'Polyline':

            # make a copy of the input to avoid editing original data
            input_copy = arcpy.ExportFeatures_conversion(input_features, gdb + r'\input_copy')

            # 2a: Generalize - reduce the number of vertices
            arcpy.AddMessage("2a. Generalizing...")
            arcpy.Generalize_edit(input_copy, tolerance="25 Meters")

            # 2b: Densify project lines so there is a vertex every 100m
            arcpy.AddMessage("2b. Densifying...")

            arcpy.Densify_edit(in_features=input_copy,
                               densification_method='DISTANCE',
                               distance="100 Meters")

            # 3: Convert line feature vertices to points
            arcpy.AddMessage("3. Line vertices to points...")

            input_points = arcpy.FeatureVerticesToPoints_management(
                in_features=input_copy,
                out_feature_class=gdb + r'\input_lines_vertices')

        ##############################################################################################
        # FROM HERE ON, POINTS AND LINES ARE IDENTICAL
        main(input_points=input_points,
             project_id_field=project_id_field,
             solver_object=solver_object,
             network_dataset=network_dataset,
             demo_geometry_feat=demo_geometry_feat,
             output_tables=output_tables,
             gdb=gdb,
             output_to_map=output_to_map,
             network_step=network_step,
             census_step=census_step,
             input_poly_buf=input_poly_buf,
             input_network_poly_buf=input_network_poly_buf
             )

    else:
        arcpy.AddMessage('Invalid geometry type in Input Features.')
        raise Exception


if __name__ == '__main__':
    input_features = arcpy.GetParameterAsText(0)
    out_gdb = arcpy.GetParameterAsText(1)
    project_id_field = arcpy.GetParameterAsText(2)
    cutoffs = arcpy.GetParameterAsText(3)
    network_dataset = arcpy.GetParameterAsText(4)
    network_segments = arcpy.GetParameterAsText(5)
    demo_geometry_feat = arcpy.GetParameterAsText(6)
    output_tables = arcpy.GetParameterAsText(7)
    output_to_map = arcpy.GetParameterAsText(8)

    arcpy.AddMessage('input_features: ' + input_features)
    arcpy.AddMessage('out_gdb: ' + out_gdb)
    arcpy.AddMessage('project_id_field: ' + project_id_field)
    arcpy.AddMessage('cutoffs: ' + cutoffs)
    arcpy.AddMessage('network_dataset: ' + network_dataset)
    arcpy.AddMessage('network_segments: ' + network_segments)
    arcpy.AddMessage('demo_geometry_feat: ' + demo_geometry_feat)
    arcpy.AddMessage('output_tables: ' + output_tables)
    arcpy.AddMessage('output_to_map: ' + output_to_map)

    ScriptTool(input_features, out_gdb, project_id_field, cutoffs, network_dataset, network_segments, demo_geometry_feat, output_tables,
               output_to_map)


