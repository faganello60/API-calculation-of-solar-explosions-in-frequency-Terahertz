package com.tcc.faganello.jsonOutPut;

import java.util.ArrayList;

public class GeneralModel {
	ArrayList<Double> frequency;
	ArrayList<Double> flux;
	
	public GeneralModel() {
		this.frequency = new ArrayList<Double>();
		this.flux = new ArrayList<Double>();
	}
	
	public void addFrequency(Double d){
		this.frequency.add(d);
	}
	
	public void addFlux(Double d){
		this.flux.add(d);
	}

	public ArrayList<Double> getFrequency() {
		return frequency;
	}

	public void setFrequency(ArrayList<Double> frequency) {
		this.frequency = frequency;
	}

	public ArrayList<Double> getFlux() {
		return flux;
	}

	public void setFlux(ArrayList<Double> flux) {
		this.flux = flux;
	}
	
}
