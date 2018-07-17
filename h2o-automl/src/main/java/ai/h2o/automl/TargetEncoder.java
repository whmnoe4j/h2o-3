package ai.h2o.automl;

import water.DKV;
import water.Key;
import water.fvec.Frame;
import water.fvec.Vec;
import water.rapids.Rapids;
import water.rapids.Val;
import water.rapids.vals.ValFrame;
import water.util.TwoDimTable;

import java.util.Arrays;

// TODO probably should call this logic from FrameUtils
public class TargetEncoder {

    public static class HoldoutType {
        public static final byte LeaveOneOut  =  0;
        public static final byte KFold  =  1;
        public static final byte None  =  2;
    }

    /**
     *
     * @param data
     * @param columnIndexesToEncode
     * @param targetIndexAsStr target column index
     * @param foldColumn should contain index of column as String. TODO Change later into suitable type.
     */
    //TODO do we need to do this preparation before as a separate phase? because we are grouping twice.
    //TODO At least it seems that way in the case of KFold. But even if we need to preprocess for other types of TE calculations... we should not affect KFOLD case anyway.
    public Frame prepareEncodingMap(Frame data, String[] columnIndexesToEncode, String targetIndexAsStr, String foldColumn) {

        //Validate input data. Not sure whether we should check some of these.

        if(data == null) throw new IllegalStateException("Argument 'data' is missing, with no default");

        if(columnIndexesToEncode == null || columnIndexesToEncode.length == 0)
            throw new IllegalStateException("Argument 'columnsToEncode' is not defined or empty");

        if(targetIndexAsStr == null || targetIndexAsStr.equals(""))
            throw new IllegalStateException("Argument 'target' is missing, with no default");

        if(! checkAllTEColumnsAreCategorical(data, columnIndexesToEncode))
            throw new IllegalStateException("Argument 'columnsToEncode' should contain only indexes of categorical columns");

        int targetIndex = Integer.parseInt(targetIndexAsStr);
        if(! data.vec(targetIndex).isCategorical() ) {
            throw new IllegalStateException("Argument 'target' should be categorical");
        } else {
            Vec targetVec = data.vec(targetIndex);
            if(targetVec.cardinality() == 2) {
                transformBinaryTargetColumn(data, targetIndex);
            }
            else {
                throw new IllegalStateException("`target` must be a numeric or binary vector");
            }
        }

        if(Arrays.asList(columnIndexesToEncode).contains(targetIndexAsStr)) {
            throw new IllegalArgumentException("Columns for target encoding contain target column.");
        }

        // Maybe we should not convert here anything because API for JAVA backend is unambiguous and takes only names of columns.

            /* Case when `columnsToEncode` columns are specified with indexes not names. Replace indexes with names
            if (is.numeric(unlist(x))) {
                x <- sapply(x, function(i) colnames(data)[i]) //TODO btw this is maybe not the very efficient way to get all column names in R.
                //TODO We should be able take them at once (not one by one).
            }*/
            //TODO code goes here


            /* Replace target index by target column name.
            if (is.numeric(y)) {
                y <- colnames(data)[y]
            }*/
            //TODO code goes here

            /* Again converting index to name for fold_column.
            if (is.numeric(fold_column)) {
                fold_column <- colnames(data)[fold_column]
            }*/
            //TODO code goes here

        // Encoding part

        // 1) For encoding we can use only rows with defined target column
        // R:  encoding_data <- data[!is.na(data[[y]]), ]

        filterOutNAsFromTargetColumn(data, targetIndexAsStr);

        // 2) Iterating over the encoding columns, grouping and calculating `numerator` and `denominator` for each group.
        // for (cols in x) {   TODO in R: cols -> col ?

        Frame allColumnsEncodedFrame = null;

        for ( String te_column: columnIndexesToEncode) {
            Frame teColumnFrame = null;
            int colIndex = Integer.parseInt(te_column);
            String tree = null;
            if(foldColumn == null) {
                tree = String.format("(GB %s [%d] sum %s \"all\" nrow %s \"all\")", data._key, colIndex, targetIndexAsStr, targetIndexAsStr);
            }
            else {
                tree = String.format("(GB %s [%d, %s] sum %s \"all\" nrow %s \"all\")", data._key, colIndex, foldColumn, targetIndexAsStr, targetIndexAsStr);
            }
            Val val = Rapids.exec(tree);
            teColumnFrame = val.getFrame();

            teColumnFrame = renameColumn(teColumnFrame, "2", "numerator");
            teColumnFrame = renameColumn(teColumnFrame, "3", "denominator");

            if(allColumnsEncodedFrame == null)
                allColumnsEncodedFrame = teColumnFrame;
            else
                allColumnsEncodedFrame.add(teColumnFrame); // TODO should we CBind frames or it is cheaper to collect an array/map ?
        }

        Key<Frame> inputForTargetEncoding = Key.make("inputForTargetEncoding");
        allColumnsEncodedFrame._key = inputForTargetEncoding;  // TODO should we set key here?
        DKV.put(inputForTargetEncoding, allColumnsEncodedFrame);

        return allColumnsEncodedFrame;
    };

    public Frame renameColumn(Frame fr, String indexOfColumnToRename, String newName) {
        String[] names = fr.names();
        names[Integer.parseInt(indexOfColumnToRename)] = newName;
        fr.setNames(names);
        return fr;
    }

    public int getColumnIndexByName(Frame fr, String name) {
        String[] names = fr.names();
        return Arrays.asList(names).indexOf(name);
    }

    public Frame filterOutNAsFromTargetColumn(Frame data, String targetIndex)  {
        // Option 1 ?
        // Why the name of the method is DEEP-select? Is it really that deep? Or it is just usual select?

        //      Frame result = new Frame.DeepSelect().doAll(Vec.T_CAT, data).outputFrame();

        // Option 2
        String tree = String.format("(rows %s  (!! (is.na (cols %s [%s] ) ) ) )", data._key, data._key, targetIndex);
        Val val = Rapids.exec(tree);
        if (val instanceof ValFrame)
            data = val.getFrame();

        return data;
    }

    public Frame transformBinaryTargetColumn(Frame data, int targetIndex)  {

        Vec targetVec = data.vec(targetIndex);
        String[] domains = targetVec.domain();
//        (:= RTMP_sid_b91f_9 (ifelse (is.na (cols RTMP_sid_b91f_9 [1.0] ) ) NA (ifelse (== (cols RTMP_sid_b91f_9 [1.0] ) 0 ) 0.0 1.0 ) ) [1.0] [] )
        //(tmp= RTMP_sid_8c98_1 (:= data (ifelse (< (cols data [4.0] ) 6.0 ) 666.0 (cols data [4.0] ) ) [4.0] [] ) )
        String tree = String.format("(:= %s (ifelse (is.na (cols %s [%d] ) ) NA (ifelse (== (cols %s [%d] ) '%s' ) 1.0 0.0 ) )  [%d] [] )",
                data._key, data._key, targetIndex,  data._key, targetIndex, domains[1], targetIndex);
        Val val = Rapids.exec(tree);
        Frame res = val.getFrame();

        return res;
    }

    public Frame getOutOfFoldData(Frame data, String foldColumn, long currentFoldValue)  {

        String tree = String.format("(rows %s (!= (cols %s [%s] ) %d ) )", data._key, data._key, foldColumn, currentFoldValue);
        Val val = Rapids.exec(tree);
        Frame outOfFoldFrame = val.getFrame();
        Key<Frame> outOfFoldKey = Key.make(data._key.toString() + "_outOfFold-" + currentFoldValue);
        outOfFoldFrame._key = outOfFoldKey;
        DKV.put(outOfFoldKey, outOfFoldFrame);
        return outOfFoldFrame;
    }

    private long[] getUniqueValuesOfTheFoldColumn(Frame data, int columnIndex) {
        String tree = String.format("(unique (cols %s [%d]))", data._key, columnIndex);
        Val val = Rapids.exec(tree);
        printOutFrameAsTable(val.getFrame());
        Vec uniqueValues = val.getFrame().vec(0);
        int length = (int) uniqueValues.length(); // We assume that fold column should not has many different values and we will fit into node's memory
        long[] uniqueValuesArr = new long[length];
        for(int i = 0; i < uniqueValues.length(); i++) {
            uniqueValuesArr[i] = uniqueValues.at8(i);
        }
        return uniqueValuesArr;
    }

    private boolean checkAllTEColumnsAreCategorical(Frame data, String[] columnsToEncode)  {
        boolean containsOnlyCategoricalColumns = true;
        int columnsToEncodeInterationIndex = 0;
        while (containsOnlyCategoricalColumns && columnsToEncodeInterationIndex < columnsToEncode.length) {
            int indexOfColumn = Integer.parseInt( columnsToEncode[columnsToEncodeInterationIndex]);
            containsOnlyCategoricalColumns = data.vec(indexOfColumn).isCategorical();
            columnsToEncodeInterationIndex += 1 ;
        }
        return containsOnlyCategoricalColumns;
    }

    public Frame groupByTEColumnAndAggregate(Frame outOfFold, String teColumnIndex, String numeratorColumnIndex, String denominatorColumnIndex) {
        String tree = String.format("(GB %s [%s] sum %s \"all\" sum %s \"all\")", outOfFold._key, teColumnIndex, numeratorColumnIndex, denominatorColumnIndex);
        Val val = Rapids.exec(tree);
        Frame resFrame = val.getFrame();
        Key<Frame> key = Key.make(outOfFold._key.toString() + "_groupped");
        resFrame._key = key;
        DKV.put(key, resFrame);
        return resFrame;
    }

    public Frame rBind(Frame a, Frame b) {
        Frame rBindRes = null;
        if(a == null) {
            assert b != null;
            rBindRes = b;
        } else {
            String tree = String.format("(rbind %s %s)", a._key, b._key);
            Val val = Rapids.exec(tree);
            rBindRes = val.getFrame();
        }
        Key<Frame> key = Key.make("holdoutEncodeMap");
        rBindRes._key = key;
        DKV.put(key, rBindRes);
        return rBindRes;
    }

    public Frame mergeBy(Frame a, Frame b, String teColumnIndexOriginal, String foldColumnIndexOriginal, String teColumnIndex, String foldColumnIndex ) {
        String tree = String.format("(merge %s %s TRUE FALSE [%s, %s] [%s, %s] 'auto' )", a._key, b._key, teColumnIndexOriginal, foldColumnIndexOriginal, teColumnIndex, foldColumnIndex);
        Val val = Rapids.exec(tree);
        Frame res = val.getFrame();
        res._key = a._key;
        DKV.put(a._key, res);
        return res;
    }

    public Frame appendColumn(Frame a, long columnValue, String appendedColumnName ) {
        String tree = String.format("( append %s %d '%s' )", a._key , columnValue, appendedColumnName);
        Val val = Rapids.exec(tree);
        Frame withAppendedColumn = val.getFrame();
        withAppendedColumn._key = a._key;  // TODO should we set key here?
        DKV.put(a._key, withAppendedColumn);
        return withAppendedColumn;
    }

    public Frame appendCalculatedTEEncoding(Frame a, String numeratorIndex, String denominatorIndex, String appendedColumnName ) {
        String tree = String.format("( append %s ( / (cols %s [%s]) (cols %s [%s])) '%s' )",a._key , a._key,  numeratorIndex, a._key, denominatorIndex, appendedColumnName);
        Val val = Rapids.exec(tree);
        Frame res = val.getFrame();
        res._key = a._key;
        DKV.put(a._key, res);
        return res;
    }

    public Frame addNoise(Frame fr, String applyToColumnName, double noiseLevel, double seed) {
        int appyToColumnIndex = getColumnIndexByName(fr, applyToColumnName);
        String tree = String.format("(:= %s (+ (cols %s [%d] ) (- (* (* (h2o.runif %s %f ) 2.0 ) %f ) %f ) ) [%d] [] )", fr._key, fr._key, appyToColumnIndex, fr._key, seed, noiseLevel, noiseLevel, appyToColumnIndex);
        Val val = Rapids.exec(tree);
        return val.getFrame();
    }

    public Frame applyTargetEncoding(Frame data,
                                     String[] columnIndexesToEncode,
                                     String targetColumnIndex,
                                     Frame targetEncodingMap, // TODO should be a Map( te_column -> Frame )
                                     byte holdoutType,
                                     String foldColumnIndex,
                                     boolean withBlendedAvg,
                                     double noiseLevel,
                                     double seed) {

        if(holdoutType == HoldoutType.KFold && foldColumnIndex == null)
            throw new IllegalStateException("`foldColumn` must be provided for holdoutType = KFold");

        if(noiseLevel < 0 )
            throw new IllegalStateException("`noiseLevel` must be non-negative");

        //TODO add validation checks as in preparation phase. Validation and test frames should comply with the same requirements as training ones.

        //TODO Should we remove string columns from `data` as it is done in R version (see: https://0xdata.atlassian.net/browse/PUBDEV-5266) ?

        Frame teFrame = data;

        for ( String teColumnIndex: columnIndexesToEncode) {
            Frame holdoutEncodeMap = null;

            switch( holdoutType ) {
                case HoldoutType.KFold:
                    // I assume here that fold column is numerical not categorical. Otherwise we could calculate it with following piece of code.
                    // String[] folds = targetEncodingMap.vec(Integer.parseInt(foldColumn)).domain();
                    long[] foldValues = getUniqueValuesOfTheFoldColumn(targetEncodingMap, Integer.parseInt("1")); // "1" fold column in targetEncodingMap

                    for(long foldValue : foldValues) { // TODO what if our te column is not represented in every foldValue? Then when merging with original dataset we will get NA'a on the right side
                        Frame outOfFoldData = getOutOfFoldData(targetEncodingMap, "1", foldValue); // In targetEncodingMap it is always 1st column
                        System.out.println(" #### OutOfFold dataframe before grouping");
                        printOutFrameAsTable(outOfFoldData);
                        Frame groupedByTEColumnAndAggregate = groupByTEColumnAndAggregate(outOfFoldData, teColumnIndex, "2", "3");
                        System.out.println(" #### groupedByTEColumnAndAggregate dataframe");
                        printOutFrameAsTable(groupedByTEColumnAndAggregate);
                        groupedByTEColumnAndAggregate = appendColumn(groupedByTEColumnAndAggregate, foldValue, "foldValueForMerge"); // TODO for now we don't need names for columns since we are working with indices

                        holdoutEncodeMap = rBind(holdoutEncodeMap, groupedByTEColumnAndAggregate);

                        TwoDimTable twoDimTable = holdoutEncodeMap.toTwoDimTable();
                        System.out.println(String.format("Column with foldValueForMerge=%d were appended.", foldValue) + twoDimTable.toString());
                    }

                    break;
                case HoldoutType.LeaveOneOut:
                    break;
                case HoldoutType.None:
                default:
            }


                System.out.println(" #### Merging holdoutEncodeMap to teFrame");

                printOutFrameAsTable(teFrame);

                printOutFrameAsTable(holdoutEncodeMap);

            teFrame = mergeBy(teFrame, holdoutEncodeMap, teColumnIndex, foldColumnIndex, "0", "3");

                System.out.println(" #### After merging teFrame");

                printOutFrameAsTable(teFrame);

            teFrame = appendCalculatedTEEncoding(teFrame, "4", "5", "target_encode" + teColumnIndex);


                System.out.println(" #### After appending calculated TE encoding");

                printOutFrameAsTable(teFrame);

        }

        if(noiseLevel > 0) {
            teFrame = addNoise(teFrame, "target_encode0", noiseLevel, seed);
        }

        return teFrame;
    }

    public Frame applyTargetEncoding(Frame data,
                                     String[] columnsToEncode,
                                     String targetIndexAsStr,
                                     Frame targetEncodingMap,
                                     byte holdoutType,
                                     String foldColumn,
                                     boolean withBlendedAvg) {
        double defaultNoiseLevel = 0.01;
        double noiseLevel = 0.0;
        int targetIndex = Integer.parseInt(targetIndexAsStr);
        Vec targetVec = data.vec(targetIndex);
        if(targetVec.isNumeric()) {
            noiseLevel = defaultNoiseLevel * (targetVec.max() - targetVec.min());
        } else {
            noiseLevel = defaultNoiseLevel;
        }
        return this.applyTargetEncoding(data, columnsToEncode, targetIndexAsStr, targetEncodingMap, holdoutType, foldColumn, withBlendedAvg, noiseLevel, 1234.0);
    }

    // TODO remove.
    private void printOutFrameAsTable(Frame fr) {

        TwoDimTable twoDimTable = fr.toTwoDimTable();
        System.out.println(twoDimTable.toString());
    }
}
