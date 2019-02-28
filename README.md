econowcast
==========

Experimental tools for Big Data econometrics nowcasting and early estimates.
---

**About**

The code source files provided herein will enable you to reproduce the experiments 
on Big Data econometrics nowcasting and early estimates presented in _Eurostat_ **Handbook on Rapid Estimates**
(_cite this source code or the reference's doi: [10.2785/488740](http://dx.doi.org/10.2785/488740)_).
Further details are also available in the other associated working papers (see 
Kapetanios _et al._'s publications [below](#References)).

<table align="center">
    <tr> <td align="left"><i>authors</i></td> <td align="left"> <a href="mailto:fotis.papailias@quantf.com">F.Papailias</a>, 
	<a href="mailto:kapetaniosgeorge@gmail.com">G.Kapetanios</a>, <a href="mailto:massimiliano.marcellino@unibocconi.it">M.Marcellino</a>, 
	<a href="mailto:katerina.petrova@st-andrews.ac.uk">K.Petrova</a></td> </tr> 
    <tr> <td align="left"><i>version</i></td> <td align="left">1.0</td> </tr> 
    <tr> <td align="left"><i>since</i></td> <td align="left">2017</td> </tr> 
    <tr> <td align="left"><i>license</i></td> <td align="left"><a href="https://joinup.ec.europa.eu/sites/default/files/eupl1.1.-licence-en_0.pdfEUPL">EUPL</a> <i>(cite the source code or the reference above!)</i></td> </tr> 
</table>

**Description**

The source code is organised in 4 distinct folders:
* [**_extract/_**](extract): Methods for feature extraction of various Big Data sources to usable time-series for econometric modelling. The scripts enable to convert unstructured datasets into structured time-series for different types of Big Data sources, _e.g._: Google searches (_Google Trends_, _Google Correlates_), social network activities (_Twitter_), mobile phone data, IoT sensors, etc...
* [**_filter/_**](filter): Filtering techniques for high frequency data. It contains some signal extraction/decomposition techniques in order to remove seasonal, very high frequency periodicity and deterministic phenomena which are not relevant for nowcasting exercises. It covers in particular outliers' detection.
* [**_model/_**](model): Relevant econometric modelling techniques for Big Data have been identified and some implementations are made available. A particular attention is set ipon Bayesian ones, _e.g._ the possibility of  using Bayesian panel VAR models, quantile regression model and expectile regression models for dealing with Big Data.
* [**_nowcast/_**](nowcast): Modelling strategies for nowcasting/early estimates purposes taking into account various Big data characteristics have been elaborated. Scripts that operate some empirical test on possible timeliness gains when using _Google Trends_, other easily accessible big data and macroeconomic and financial variables are provided. Accuracy gains through improving the timeliness of the selected variables at the beginning, middle and end of the reference period together with the associated accuracy loss are also investigated.

The results presented in the various publications referred to [below](#References) can be reproduced.
For that purpose, the necessary raw (as well as the output data) are made available to the user in the [**_data/_**](data) folder.
Further (narrative) description of the various functions/scripts is also provided in this [document](docs/econowcast-code_description.pdf), located in the [**_docs/_**](docs) folder, including the [evaluation of the nowcasting/flash estimation techniques](docs/Nowcasting_Flash_estimation_evaluation) based on a big set of indicators.

**<a name="References"></a>References** 

*  M.G.Marcellino, F.Papailias, G.L.Mazzi, G.Kapetanios, George and D.Buono (2018): **Big Data econometrics: Now casting and early estimates**, no. 2018-82, BAFFI CAREFIN Centre Research, ssrn:[3206554](https://ssrn.com/abstract=3206554).

* G.Kapetanios, M.Marcellino, F.Papailias (2017): 
**Guidance and recommendations on the use of Big data for macroeconomic nowcasting** in
[**Handbook on Rapid Estimates**](http://ec.europa.eu/eurostat/documents/3859598/8555708/KS-GQ-17-008-EN-N.pdf), Chapter 17,
_Publications Office of the European Union_, doi:[10.2785/4887400](http://dx.doi.org/10.2785/4887400).  

* G.Kapetanios, M.Marcellino, F.Papailias (2017): 
[**Filtering techniques for big data and big data based uncertainty indexes**](http://ec.europa.eu/eurostat/documents/3888793/8440791/KS-TC-17-007-EN-N.pdf),
_Publications Office of the European Union_, doi:[10.2785/880943](http://dx.doi.org/10.2785/880943).

* G.Kapetanios, M.Marcellino, F.Papailias (2017): 
[**Big data conversion techniques including their main features and characteristics**](http://ec.europa.eu/eurostat/documents/3888793/8123371/KS-TC-17-003-EN-N.pdf), 
_Publications Office of the European Union_, doi:[10.2785/461700](http://dx.doi.org/10.2785/461700).

* E. Baldacci, D. Buono, G. Kapetanios, S. Krische, M. Marcellino, G.L. Mazzi, F. Papailias (2016): 
[**Big Data and macroeconomic nowcasting: From data access to modelling**](http://ec.europa.eu/eurostat/documents/3888793/7753027/KS-TC-16-024-EN-N.pdf),
_Publications Office of the European Union_, doi:[10.2785/360587](http://dx.doi.org/10.2785/360587).

* G.L. Mazzi, F. Moauro, R. Ruggeri Cannata (2016): 
[**Advances in econometric tools to complement official statistics in the field of Principal European Economic Indicators**](http://ec.europa.eu/eurostat/documents/3888793/7579703/KS-TC-16-013-EN-N.pdf/21b94a6c-55ba-4d3a-af52-01617bbe4310),
_Publications Office of the European Union_, doi:[10.2785/397407](http://dx.doi.org/10.2785/397407).
