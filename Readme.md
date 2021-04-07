# Simulation of Public Procurement Networks
## _Term project for course Networks & Markets_

This repository corresponds to an algorithm and a set of functions which together implement a simulation model of the market structure and competition outcomes for a market of public contracts.

The objective of the project is twofold. The first objective is to fit a model which can render accurate predictions of how outcomes would change if the number of nodes were to change(i.e. entry and exit of firms). The second objective is to generate counterfactuals predictions given scenarios of increased entry and network density. 

## Setting
We have a set of N buyers(government units) and M sellers(private firms) where the product sold corresponds to public contracts for construction projects. The market structure is a bipartite network, where the set of top nodes are the govt. units and the set of bottom nodes are the private firms. The sellers can only provide goods to the buyers they are connected to in the network. Given a bipartite network, competition for projects is done via first price, sealed bid auctions. Every government unit can hold an auction, to which a subset of its connected firms submit bids. The firm which submits the lowest bid wins the contract. 

We are mainly interested in how effiency can be related to several types of market structure. Thus, our main quantity of interest is the average bid in the auctions. This should also be extended to consider the winning bid. 

## Data
We employ a datatset of aprox. 60,000 auctions for public construction projects in Chile. This dataset contains aprox. 600 govt units and 7,000 firms. Each observation corresponds to a bid submitted by firm f to contract c auctioned by government unit g. Importantly, the data includes an oficcial estimate of the cost of the project. Throughout the project, we divide bids by this quantity in order to produce comparable estimates across different contracts. 

## Algorithm 
The simulation consists in the following elements : 
- A simulation of the network of the buyers and sellers, i.e. which firms can supply to which buyers
- A simulation of auctions within each government unit. This stage in the simulation produces:
   - A set of firm who bid for the project
   - Bids amounts for each of the previous firms

We implement two different models for the second stage. The first is exogenous, while the second is endogenous., 

## Results
The first objective is to validate that the model can adequaterly simulate predictions regarding competition outcomes. Thus, I first fit the model parameters employing a two year period of data . Then, I simulate the next two year period, comparing the outcomes obtained with the two different models.  

The second objective is to generate counterfactuals. The scenarios analyzed correspond to increased entry and network density. 

## Organization of the repository


## License

MIT



[//]: # (These are reference links used in the body of this note and get stripped out when the markdown processor does its job. There is no need to format nicely because it shouldn't be seen. Thanks SO - http://stackoverflow.com/questions/4823468/store-comments-in-markdown-syntax)

   [dill]: <https://github.com/joemccann/dillinger>
   [git-repo-url]: <https://github.com/joemccann/dillinger.git>
   [john gruber]: <http://daringfireball.net>
   [df1]: <http://daringfireball.net/projects/markdown/>
   [markdown-it]: <https://github.com/markdown-it/markdown-it>
   [Ace Editor]: <http://ace.ajax.org>
   [node.js]: <http://nodejs.org>
   [Twitter Bootstrap]: <http://twitter.github.com/bootstrap/>
   [jQuery]: <http://jquery.com>
   [@tjholowaychuk]: <http://twitter.com/tjholowaychuk>
   [express]: <http://expressjs.com>
   [AngularJS]: <http://angularjs.org>
   [Gulp]: <http://gulpjs.com>

   [PlDb]: <https://github.com/joemccann/dillinger/tree/master/plugins/dropbox/README.md>
   [PlGh]: <https://github.com/joemccann/dillinger/tree/master/plugins/github/README.md>
   [PlGd]: <https://github.com/joemccann/dillinger/tree/master/plugins/googledrive/README.md>
   [PlOd]: <https://github.com/joemccann/dillinger/tree/master/plugins/onedrive/README.md>
   [PlMe]: <https://github.com/joemccann/dillinger/tree/master/plugins/medium/README.md>
   [PlGa]: <https://github.com/RahulHP/dillinger/blob/master/plugins/googleanalytics/README.md>
