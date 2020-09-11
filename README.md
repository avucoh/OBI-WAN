# Ontology for Biological Investigations - With Applications for nPOD (OBI-WAN)

OBI-WAN is an application-specific ontology in development that reuses the OBI ontology to support nPOD data annotation and produce open linked data. To clarify its application-specific uses, we'll give a brief overview of the motivation for and development of OBI-WAN thus far:

First, it is important to know that nPOD (the Network for Pancreatic Organ Donors), est. 2009, is a diabetes-focused biosample bank that distributes organ tissues to investigators world-wide for them to conduct assays that help answer important and often diverse biological questions.

As emphasis grew on the merits of data integration and FAIR data sharing, there was a realization and massive effort to collect the datasets that have been published on the nPOD samples, and to annotate and standardize them for secondary analyses or other re-use. Because the samples have been the input to many different types of biological investigations, OBI was naturally chosen as the source of terms for annotating the collected datasets given its suitability and established use in numerous other projects.

Despite the name, OBI-WAN doesn't include just OBI. To further enrich data annotation, relevant parts of other ontologies also became part of OBI-WAN over time (these conceptually discrete parts are commonly referred to as modules).

What makes OBI-WAN unique is of course the terms that are somewhat specific to the nPOD domain that we've had to create to conceptualize entities/relationships important to the nPOD domain (previously undefined or poorly defined elsewhere). For example, there are Theme class terms that reflect biological themes studied by nPOD working groups and classes to categorize data with nPOD-defined classifications. Because the ontology is meant to facilitate search in a graph database, much of the development is influenced by the Uniprot Core ontology used for the Uniprot database.


## Products

obi-wan-base.ttl contains the basic application-specific classes.
obi-wan.ttl contains the application-specific classes with module imports merged in.

## See Also

The primary respository is https://github.com/avucoh/OBI-WAN, but it is mirrored on [Gitlab](https://gitlab.com/npod/obi-wan), where it is organized with other products within https://gitlab.com/npod/.


