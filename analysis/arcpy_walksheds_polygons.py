import arcpy


def polygon_analysis(input_polygons, project_id_field, network_segments,
               demo_geometry_feat, output_tables, gdb):

    """
    :param input_polygons: Path to input polygons
    :param project_id_field: Field containing ID (str) for groups of locations (i.e. a TIP project)
    :param network_segments: An ESRI dataset containing a network dataset and segments feature class
    :param demo_geometry_feat: Dictionary with structure: {[path to census features (tracts or block groups)]: list["geoid field name", "census geom (tract or bg)"]}
    :param output_tables: List of output tables corresponding to demographic geometries above
    :param gdb: Geodatabase where intermediate values will be saved
    :return:
    """

    demo_geom_layer = 'demo_geom_layer'
    network_segs_layer = 'network_segs_layer'
    network_segs_buffer_iden_layer = 'network_segs_buffer_iden_layer'
    network_segs_buffer_fc = gdb + r'\network_segs_buffer'
    network_segs_buffer_iden_fc = gdb + r'\network_segs_buffer_iden'
    network_segs_buffer_inter_proj_fc = gdb + r'\network_segs_buffer_inter_proj'
    network_segs_buffer_inter_diss = gdb + r'\network_segs_buffer_inter_diss'
    network_segs_buffer_joined_table = gdb + r'\network_segs_buffer_joined_table'

    project_id_field_blank = False
    if project_id_field == '':
        arcpy.AddMessage('Assigning project ID field...')
        project_id_field = 'ID_field'
        project_id_field_blank = True


    # If all buffers should be dissolved together:
    if project_id_field_blank:
        # Add project ID field to points so that output line of service area have the right names (not just 'Location ###')
        arcpy.AddMessage(f"Adding project ID field ({project_id_field}) to input points...")
        arcpy.AddField_management(input_polygons, field_name=project_id_field, field_type='TEXT')
        arcpy.CalculateField_management(in_table=input_polygons,
                                        field=project_id_field,
                                        expression=f"'{project_id_field}'",
                                        expression_type='PYTHON3')

    i = 1
    for dataset in demo_geometry_feat:

        geom = dataset
        geoid_field = demo_geometry_feat[geom][0]
        geom_str = demo_geometry_feat[geom][1]

        # 1. Select demographics polygons that intersect the input polygon features
        arcpy.AddMessage("1. Selecting demographics polygons...")

        arcpy.MakeFeatureLayer_management(in_features=geom,
                                          out_layer=demo_geom_layer)
        arcpy.SelectLayerByLocation_management(in_layer=demo_geom_layer,
                                               overlap_type="INTERSECT",
                                               select_features=input_polygons)

        # 2. Select road network segments in demographics polygons
        arcpy.AddMessage("2. Selecting road network segments...")

        arcpy.MakeFeatureLayer_management(in_features=network_segments,
                                          out_layer=network_segs_layer)
        arcpy.SelectLayerByLocation_management(in_layer=network_segs_layer,
                                               overlap_type='INTERSECT',
                                               select_features=demo_geom_layer)

        # 3. Create polygon buffer around road network
        arcpy.AddMessage('3. Creating polygon buffer around road network segments...')

        arcpy.Buffer_analysis(in_features=network_segs_layer,
                              buffer_distance_or_field="5 Meters",
                              out_feature_class=network_segs_buffer_fc,
                              dissolve_option='ALL')

        # 4. Identity road network buffer using demo polygons
        arcpy.AddMessage("4. Identifying road network buffer using demo polygons...")

        arcpy.Identity_analysis(in_features=network_segs_buffer_fc,
                                identity_features=geom,
                                out_feature_class=network_segs_buffer_iden_fc,
                                join_attributes="ALL")

        # 5. Intersect ID'd network buffer and input polygons
        arcpy.AddMessage("5. Intersecting ID'd network buffer using input polygons...")

        arcpy.Intersect_analysis(in_features=[network_segs_buffer_iden_fc, input_polygons],
                                 out_feature_class=network_segs_buffer_inter_proj_fc)

        # Dissolve intersected buffer on geoid and project id to avoid slivers. We want one featue for each
        # project-geoid combination, otherwise we will get multiple area fractions in the output.
        arcpy.AddMessage("Dissolving intersected buffer on geoid and project ID...")

        arcpy.Dissolve_management(in_features=network_segs_buffer_inter_proj_fc,
                                  out_feature_class=network_segs_buffer_inter_diss,
                                  dissolve_field=[geoid_field, project_id_field])

        # 6. Join ID'd road network and ID'd road network intersected with project polygons
        arcpy.AddMessage("6. Joining road network layers...")

        arcpy.MakeFeatureLayer_management(in_features=network_segs_buffer_iden_fc,
                                          out_layer=network_segs_buffer_iden_layer)
        arcpy.AddJoin_management(in_layer_or_view=network_segs_buffer_iden_layer,
                                 in_field=geoid_field,
                                 join_table=network_segs_buffer_inter_diss,
                                 join_field=geoid_field,
                                 join_type='KEEP_COMMON')

        # 7. Export joined table
        arcpy.AddMessage("7. Exporting joined table...")

        arcpy.env.qualifiedFieldNames = False

        arcpy.ExportTable_conversion(in_table=network_segs_buffer_iden_layer,
                                     out_table=network_segs_buffer_joined_table)

        # 8: Add area_fraction field
        arcpy.AddMessage("8. Adding area_fraction field...")

        arcpy.AddField_management(in_table=network_segs_buffer_joined_table,
                                  field_name='area_fraction',
                                  field_type='DOUBLE')

        # 9: Calculate area_fraction field
        arcpy.AddMessage("9. Calculating area_fraction field...")

        iden_area_field = "!shape_Area!"
        proj_area_field = "!shape_Area_1!"

        arcpy.CalculateField_management(in_table=network_segs_buffer_joined_table,
                                        field='area_fraction',
                                        expression=proj_area_field + r'/' + iden_area_field,
                                        expression_type='PYTHON3')

        # 10. Export table to csv - fancy field mapping to make the output table clean
        arcpy.AddMessage("10. Exporting to output table...")

        arcpy.env.qualifiedFieldNames = False

        output_fields = [network_segs_buffer_joined_table + '.' + project_id_field,
                         network_segs_buffer_joined_table + '.' + geoid_field,
                         network_segs_buffer_joined_table + '.area_fraction']

        fms = arcpy.FieldMappings()

        # FieldMap objects
        project_id = arcpy.FieldMap()
        geoid = arcpy.FieldMap()
        area_fraction = arcpy.FieldMap()

        # add input fields
        project_id.addInputField(network_segs_buffer_joined_table, project_id_field)
        geoid.addInputField(network_segs_buffer_joined_table, geoid_field)
        area_fraction.addInputField(network_segs_buffer_joined_table, 'area_fraction')

        # set output fields
        project_id_name = project_id.outputField
        project_id_name.name = project_id_field
        project_id.outputField = project_id_name

        geoid_name = geoid.outputField
        geoid_name.name = 'geoid'
        geoid.outputField = geoid_name

        area_fraction_name = area_fraction.outputField
        area_fraction_name.name = 'area_fraction'
        area_fraction.outputField = area_fraction_name

        # add FieldMaps to FieldMapping object
        fms.addFieldMap(project_id)
        fms.addFieldMap(geoid)
        fms.addFieldMap(area_fraction)

        output_table = output_tables[i - 1]
        arcpy.ExportTable_conversion(in_table=network_segs_buffer_joined_table,
                                     out_table=output_table,
                                     field_mapping=fms)

        # 19. Create joined output for debugging - this is the shapes of the network buffer joined to the area fraction table

        arcpy.AddMessage('Creating joined output...')
        output_joined_lyr = arcpy.MakeFeatureLayer_management(network_segs_buffer_inter_diss, 'output_joined_lyr')
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
        proj_id_fm.addInputField(network_segs_buffer_inter_diss, project_id_field)
        geoid.addInputField(network_segs_buffer_inter_diss, geoid_field)
        area_fraction.addInputField(output_table, 'area_fraction')

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