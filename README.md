<!-- Grab your social icons from https://github.com/carlsednaoui/gitsocial -->

# Linkage of Primary Care Prescribing Records and Pharmacy Dispensing Records in the Salford Lung Study: Application in Asthma 

Welcome!  In this project I am running secondary analyses of the Salford Lung Study data: an intervention study which investigated the effectiveness of fluticasone furoate plus vilanterol on asthma control.  The paper for the original study is entitled ["Effectiveness of fluticasone furoate plus vilanterol on asthma control in clinical practice: an open-label, parallel group, randomised controlled trial."](https://www.sciencedirect.com/science/article/pii/S0140673617323978?via%3Dihub) (Woodcock et al., 2017, 69(12) Lancet).

The paper relating to this body of work has been accepted for publication in BMC Medical Research Methodology and is currently in press .  The citation and a link to the paper will be added upon publication.


Thank you for visiting,

Holly Tibble [![alt text][1.2]][1]



## Abstract
Records of medication prescriptions can be used in conjunction with pharmacy dispensing records to investigate the incidence of adherence, which is defined as observing the treatment plans agreed between a patient and their clinician.   Using prescribing records alone fails to identify primary non-adherence; medications not being collected from the dispensary.  Using dispensing records alone means that cases of conditions that resolve and/or treatments that are discontinued will be unaccounted for.  While using a linked prescribing and dispensing dataset to measure medication non-adherence is optimal, this linkage is not routinely conducted.  Furthermore, without a unique common event identifier, linkage between these two datasets is not straightforward.   

We undertook a secondary analysis of the Salford Lung Study dataset. A novel probabilistic record linkage methodology was developed matching asthma medication pharmacy dispensing records and primary care prescribing records, using semantic (meaning) and syntactic (structure) harmonization, domain knowledge integration, and natural language feature extraction.  Cox survival analysis was conducted to assess factors associated with the time to medication dispensing after the prescription was written.  Finally, we used a simplified record linkage algorithm in which only identical records were matched, for a naïve benchmarking to compare against the results of our proposed methodology.

We matched 83% of pharmacy dispensing records to primary care prescribing records.  Missing data were prevalent in the dispensing records which were not matched – approximately 60% for both medication strength and quantity.  A naïve benchmarking approach, requiring perfect matching, identified one-quarter as many matching prescribing records as our methodology.    Factors associated with delay (or failure) to collect the prescribed medication from a pharmacy included season, quantity of medication prescribed, previous dispensing history and class of medication.   Our findings indicate that over 30% of prescriptions issued were not collected from a dispensary (primary non-adherence). 

We have developed a probabilistic record linkage methodology matching a large percentage of pharmacy dispensing records with primary care prescribing records for asthma medications. This will allow researchers to link datasets in order to extract information about asthma medication non-adherence.  


[1]: https://twitter.com/HollyTibble
[1.2]: http://i.imgur.com/wWzX9uB.png
