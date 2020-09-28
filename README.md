# PMM_Nearest_Existing
Functions and Example Code for forcing predictions to be in the same Space as defined by the Train-Set.

## Background and Problem

Illogical predictions are Predictions made by AI, which do not confirm to the logical boundaries of the described problem space. This type of predictions are especially common, where multiple label dimensions are to be predicted (eg. Weather, Spare Parts).

Illogial Predictions in this domain are for example:
* Prediction of Clear Sky and snowfall at the same time, or 30°C and Ice-Warnings
* In Spare part Predictions: prediction of Spare parts usually associated with diferent types of Machinery on the same Asset.

## Proposed Solution:

1. Every dimension of the finished Label is predicted independently.
2. Predictions on the Train set are stored within the model
3. New predictions are compared to the Predictions made on the Train-Set via Nearest Neighbours.
4. The True labels of the k-closest predictions Within the Train-Set are Returned.

For further explanation and examples, see the example markdown file
