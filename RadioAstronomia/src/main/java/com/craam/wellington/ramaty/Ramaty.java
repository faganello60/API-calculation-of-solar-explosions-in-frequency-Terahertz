package com.craam.wellington.ramaty;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.craam.wellington.interfaces.IAlgorithm;
import com.craam.wellington.interfaces.IInput;
import com.craam.wellington.interfaces.IOutput;
import com.craam.wellington.ramaty.io.*;
import com.craam.wellington.statics.Constants;
import com.craam.wellington.utils.*;

public class Ramaty implements IAlgorithm {
	
	// Objetos de I/O
	InputData inputData;
	IOutput output;
	
	// Valores físicos 
	Double normalizedEnergy; // Energia normalizada
	Double plasmaFrequency; // Frequência de Plasma
	Double gammaTreshould;

	// Vetores de valores físicos
	List<Frequency> frequencies = new ArrayList<Frequency>();
	
	public Ramaty(IInput input, IOutput output){

		long start = System.nanoTime();
		// Carregando objetos de I/O
		this.inputData = (InputData) input.getInputData();
		this.output = output;
		
		// Inicializando valores
		normalizedEnergy = this.inputData.getElectronNumber() / this.NormalizeEnergy();
		plasmaFrequency = Constants.RAZIN_FFB_RATIO / this.inputData.getAlpha();
		gammaTreshould = inputData.getEnergyTreshould() / Constants.ELECTRON_REST_ENERGY + 1;
		
		// Inicia o cálculo entre os índices de frequência
		for(int i=1; i<=this.inputData.getHighestFrequency(); i++){
			frequencies.add(new Frequency(i));
		}
		
		output.generateOutput(this);
		long finish = System.nanoTime();
		System.out.println((finish-start)/1000000000.0);
	}
	
	// Normaliza a energia total para 1
	private Double NormalizeEnergy(){
		Double xnorm = 0.;
		
		for(int i=inputData.getEnergyLower(); i<=inputData.getEnergyUpper(); i++){
			Double el = Math.pow(10, (0.1 * (i-1)  -2));
			Double eu = Math.pow(10, (0.1 * (i)    -2));
			Double em = Math.pow(10, (0.1 * (i-0.5)-2));
			Double de = eu-el;
			xnorm += Math.pow(em,-inputData.getEnergyEspectrum())*de;
		}
		
		return xnorm;
	}
	
	public List<Frequency> getFrequencies(){
		return this.frequencies;
	}
	
	public InputData getInputParams(){
		return inputData;
	}
	
	public class Frequency {
		
		private int frequencyIndex;
		private Double frequencyValue;
		private RefractionPolarization refractionPolarization;
		private Radiation radiation;
		private Double ordEmissivity=0., extraordEmissivity=0.;
		private Double ordAbsorption=0., extraordAbsorption=0.;
		private StokesAndPolarization stokesAndPolarization;
		
		protected Frequency(int frequencyIndex){
			this.frequencyIndex = frequencyIndex;

			//Cálcula o valor da frequência, com base no índíce
			if(frequencyIndex <= Constants.GYROFREQUENCY_TRESHOULD)
				this.frequencyValue = Math.pow(10, (0.01*(frequencyIndex-1)));
			else
				this.frequencyValue = Math.pow(10, (0.1*(frequencyIndex-91)));
			
			this.refractionPolarization = new RefractionPolarization(this.getFrequency());
			
			// Faz a integração sobre a energia
			Double el,eu,em;
	
			for(int i=inputData.getEnergyLower(); i<=inputData.getEnergyUpper(); i++){
				el = Math.pow(10, (0.1*(i-1)-2));
				eu = Math.pow(10, (0.1*i-2));
				em = Math.pow(10, (0.1*(i-0.5)-2));
				Double de = eu-el;
				Double gamma = em / Constants.ELECTRON_REST_ENERGY + 1;
				radiation = new Radiation(gamma);
	
				ordEmissivity +=  (radiation.getOrdEmissivityIndex() * Math.pow(em, -inputData.getEnergyEspectrum()) * de * inputData.getMagneticField() * Constants.EMISSIVITY_FACTOR);
				extraordEmissivity += (radiation.getExtraordEmissivityIndex() * Math.pow(em, -inputData.getEnergyEspectrum()) * de * inputData.getMagneticField() * Constants.EMISSIVITY_FACTOR);
				ordAbsorption += (radiation.getOrdEmissivityIndex() * Math.pow(em, -inputData.getEnergyEspectrum()) * de / (Math.pow(getFrequency(), 2)) / inputData.getMagneticField() * Constants.ABSORPTION_FACTOR * (inputData.getEnergyEspectrum() * gamma *(gamma + 1 ) + 2 *Math.pow(gamma, 2) -1) / gamma / (Math.pow(gamma, 2)-1));
				extraordAbsorption += (radiation.getExtraordEmissivityIndex() * Math.pow(em, -inputData.getEnergyEspectrum()) * de / Math.pow(getFrequency(), 2) / inputData.getMagneticField() * Constants.ABSORPTION_FACTOR * (inputData.getEnergyEspectrum() * gamma *(gamma + 1 ) + 2 *Math.pow(gamma, 2) -1) / gamma / (Math.pow(gamma, 2)-1));
			}
			
			this.stokesAndPolarization = new StokesAndPolarization();

		}
		
		public int getFrequencyIndex(){
			return this.frequencyIndex;
		}
	
		public Double getFrequency(){
			return this.frequencyValue;
		}
		
		protected RefractionPolarization getRefractionPolarization(){
			return this.refractionPolarization;
		}
		
		protected Radiation getRadiation(){
			return this.radiation;
		}
		
		public StokesAndPolarization getStokesAndPolarization(){
			return stokesAndPolarization;
		}
		
		public Double getOrdEmissivity() {
			return this.ordEmissivity;
		}

		public Double getExtraordEmissivity() {
			return this.extraordEmissivity;
		}

		public Double getOrdAbsorption() {
			return this.ordAbsorption;
		}

		public Double getExtraordAbsorption() {
			return this.extraordAbsorption;
		}

		public void setExtraordAbsorption(Double extraordAbsorption) {
			this.extraordAbsorption = extraordAbsorption;
		}

		protected class RefractionPolarization {

			private Double ordRefractionIndex, extraordRefractionIndex;
			private Double ordPolarizationCoefficient, extraordPolarizationCoefficient;
			
			private RefractionPolarization(Double frequency){
			
				// Refraction Index
				Double refractionNum = 2 * Math.pow(plasmaFrequency,2) * (Math.pow(plasmaFrequency,2) - Math.pow(frequency,2));
				Double ordRefractionDnum = +Math.sqrt(Math.pow(frequency,4) * Math.pow(inputData.getViewAngleSin(),4) + 4 * Math.pow(frequency,2)*Math.pow((Math.pow(plasmaFrequency,2)-Math.pow(frequency,2)), 2) * Math.pow(inputData.getViewAngleCs(),2)) - 2 * Math.pow(frequency,2) * (Math.pow(plasmaFrequency,2) - Math.pow(frequency,2)) - Math.pow(frequency, 2) * Math.pow(inputData.getViewAngleSin(), 2);
				Double extraordRefractionDnum = -Math.sqrt(Math.pow(frequency,4) * Math.pow(inputData.getViewAngleSin(),4) + 4 * Math.pow(frequency,2)*Math.pow((Math.pow(plasmaFrequency,2)-Math.pow(frequency,2)), 2) * Math.pow(inputData.getViewAngleCs(),2)) - 2 * Math.pow(frequency,2) * (Math.pow(plasmaFrequency,2) - Math.pow(frequency,2)) - Math.pow(frequency, 2) * Math.pow(inputData.getViewAngleSin(), 2);
						
				this.ordRefractionIndex = 1 + refractionNum / ordRefractionDnum;
				this.extraordRefractionIndex = 1 + refractionNum / extraordRefractionDnum;

				// Polarization Index
				Double polarizationNum = 2 * frequency * (Math.pow(plasmaFrequency,2) - Math.pow(frequency,2)) * inputData.getViewAngleCs();
				Double ordPolarizationDnum = +Math.sqrt(Math.pow(frequency, 4) * Math.pow(inputData.getViewAngleSin(),4) + 4 * Math.pow(frequency, 2) * Math.pow((Math.pow(plasmaFrequency, 2) - Math.pow(frequency, 2)), 2)* Math.pow(inputData.getViewAngleCs(), 2)) - Math.pow(frequency,2) *  Math.pow(inputData.getViewAngleSin(),2);
				Double extraordPolarizationDnum = -Math.sqrt(Math.pow(frequency, 4) * Math.pow(inputData.getViewAngleSin(),4) + 4 * Math.pow(frequency, 2) * Math.pow((Math.pow(plasmaFrequency, 2) - Math.pow(frequency, 2)), 2)* Math.pow(inputData.getViewAngleCs(), 2)) - Math.pow(frequency,2) *  Math.pow(inputData.getViewAngleSin(),2);

			    this.ordPolarizationCoefficient = -polarizationNum / ordPolarizationDnum;
			    this.extraordPolarizationCoefficient = -polarizationNum / extraordPolarizationDnum;
			}
			

			public Double getOrdRefractionIndex(){
				return ordRefractionIndex;
			}
			public Double getExtraordRefractionIndex(){
				return extraordRefractionIndex;
			}
			public Double getOrdPolarizationCoefficient(){
				return ordPolarizationCoefficient;
			}
			public Double getExtraordPolarizationCoefficient(){
				return extraordPolarizationCoefficient;
			}

		}
		
		protected class Radiation {
			
			Double gamma;
			Double ordEmissivityIndex = 0.;
			Double extraordEmissivityIndex = 0.;
			Double refraction = 0.;
			Double polarization= 0.;

			protected Radiation(Double gamma){
				this.gamma = gamma;
				
				if(this.gamma < gammaTreshould){
					
					if(getFrequency() > plasmaFrequency){
						this.refraction = Math.sqrt(refractionPolarization.getOrdRefractionIndex());
						this.polarization = refractionPolarization.getOrdPolarizationCoefficient();
						this.ordEmissivityIndex = this.GyroSincrotron();
					}
					
					if(getFrequency() > (Math.sqrt(Math.pow(plasmaFrequency,2) + 0.25) + 0.5)){
						this.refraction = Math.sqrt(refractionPolarization.getExtraordRefractionIndex());
						this.polarization = refractionPolarization.getExtraordPolarizationCoefficient();
						this.extraordEmissivityIndex = this.GyroSincrotron();
					}
					
				} else if(this.gamma >= gammaTreshould){
						
					if(getFrequency() > (Math.sqrt(Math.pow(plasmaFrequency,2) + 0.25) + 0.5)){
						Double ssyValue = this.Synchrotron();
						this.ordEmissivityIndex = ssyValue;
						this.extraordEmissivityIndex = ssyValue;
					}
				
				}
			}
			
			
			protected Double GyroSincrotron(){
				
				Double beta = Math.sqrt(gamma*gamma-1)/gamma;
				Double ffc = getFrequency() * 2. / 3. / inputData.getViewAngleSin() / gamma / gamma;
			
				if(ffc >= 20){
					return 0.;
				} else {
					
					Double ordS = (getFrequency() * gamma * (1 - this.refraction * beta * inputData.getViewAngleCs()) +1);
					Double extraordS = (getFrequency() * gamma * (1 + this.refraction * beta * inputData.getViewAngleCs()));

					Double sum = 0.;

					for(Double i=Math.floor(ordS); i<=Math.floor(extraordS); i++){
						Double cosPhi = (1 -i/getFrequency()/gamma) / beta / inputData.getViewAngleCs() / this.refraction;
						Double sinPhi = Math.sqrt(1 -Math.pow(cosPhi, 2));
						Double xs = i * this.refraction * beta * inputData.getViewAngleSin() * sinPhi / (1 -this.refraction * beta * inputData.getViewAngleCs() * cosPhi);
						Double xstr;

						if(getFrequency() > 50){
							xstr = 0.8;
							if(gamma > 5) xstr = 0.9;
							if(gamma > 10) xstr = 0.95;
							if(gamma > 15) xstr = 0.96;
							if(xs < (xstr*i)) {
								continue;
							}
						}
						
						if(getFrequency() > 250){
							xstr = 0.9;
							if(gamma > 5) xstr = 0.92;
							if(gamma > 10) xstr = 0.97;
							if(gamma > 15) xstr = 0.98;
							if(xs < (xstr*i)){
								continue;
							}
						}

						Double bessel = MathUtils.Bessel(i.intValue(), xs);
						Double besselPr = MathUtils.BesselPr(i.intValue(), xs);
						
						Double f = Math.pow((-beta * sinPhi * besselPr + polarization * (inputData.getViewAngleCs() / inputData.getViewAngleSin() / refraction - beta * cosPhi / inputData.getViewAngleSin())*bessel),2);
						Double oldSum = sum;
						sum = sum + f;
					
						if(oldSum >0)
							if( ((sum-oldSum)/oldSum) < (1e-4)){
								break;
						}
				
					}

					return sum / beta / 2 / inputData.getViewAngleCs() * getFrequency() / (1 + Math.pow(polarization,2));
				}
			}
			
			protected Double Synchrotron(){
				Double f =0.;
				Double ffc = getFrequency()*2 / 3 / inputData.getViewAngleSin() / gamma / gamma * Math.pow((1 + 9 / 4 * (Math.pow(gamma, 2) -1) / Math.pow(inputData.getAlpha(), 2) / Math.pow(getFrequency(), 2)), 1.5);
				if(ffc <= Constants.SSY_XD[0]){
					f = 2.15*ffc*0.333*(1-0.844*ffc*0.666);
				} else if(ffc > Constants.SSY_XD[33]){
					f = 1.253 * Math.exp(-ffc) * Math.sqrt(ffc) * (1 + 55/72/ffc);
				} else {
					f = this.Sear(ffc);
				}
				return (0.138 * inputData.getViewAngleSin() * f) / 2;
			}
			
			protected Double Sear(Double ffc){
				int i=0;
				while(i < Constants.SSY_XD.length){
					if(Constants.SSY_XD[i] >= ffc) 	return (Constants.SSY_FD[i] * (ffc-Constants.SSY_XD[i-1])  + Constants.SSY_FD[i-1] * (Constants.SSY_XD[i] - ffc)) / (Constants.SSY_XD[i]-Constants.SSY_XD[i-1]);
					i++;
				}
				return 0.;
			}
			
			protected Double getOrdEmissivityIndex() {
				return this.ordEmissivityIndex;
			}

			protected Double getExtraordEmissivityIndex() {
				return this.extraordEmissivityIndex;
			}
			
			protected Double getRefraction(){
				return this.refraction;
			}

		}
		
		public class StokesAndPolarization {
			
			protected Double emissivityPolarization;
			protected Double fluxPolarization;
			protected Double phiOrd, phiExtraord, phiTotal;
			protected Double Q,V;
			
			protected StokesAndPolarization(){
				if(getExtraordEmissivity() > 0){
					this.emissivityPolarization = (getExtraordEmissivity() - getOrdEmissivity()) / (getExtraordEmissivity() + getOrdEmissivity()); 
				} else {
					this.emissivityPolarization = 0.;
				}
				
				this.phiOrd = this.Phi(getOrdEmissivity(), getOrdAbsorption());
				this.phiExtraord = this.Phi(getExtraordEmissivity(), getExtraordAbsorption());
				this.phiTotal = this.phiOrd + this.phiExtraord;
				
				this.Q = (phiOrd * (1 - Math.pow(getRefractionPolarization().getOrdPolarizationCoefficient(),2)) / (1 + Math.pow(getRefractionPolarization().getOrdPolarizationCoefficient(),2))) +
			  	    (phiExtraord * (1 - Math.pow(getRefractionPolarization().getExtraordPolarizationCoefficient(),2)) / (1 + Math.pow(getRefractionPolarization().getExtraordPolarizationCoefficient(),2)));

				this.V = 2 * (phiOrd * getRefractionPolarization().getOrdPolarizationCoefficient() / (1 + Math.pow(getRefractionPolarization().getOrdPolarizationCoefficient(), 2))) + (phiExtraord * getRefractionPolarization().getExtraordPolarizationCoefficient() / (1 + Math.pow(getRefractionPolarization().getExtraordPolarizationCoefficient(), 2)));

				if(phiTotal > 0){
					this.fluxPolarization =(phiExtraord-phiOrd) / phiTotal;
				} else this.fluxPolarization =0.;

			}
			
			protected Double Phi(Double emissivity, Double absorption){
				Double phi;
				Double arg = (normalizedEnergy / (Math.pow(Constants.UA, 2) * Math.pow(inputData.getSourceSize(), 2) / absorption));
				if(arg < 1e-3){
					phi = emissivity * normalizedEnergy / Math.pow(Constants.UA, 2);
				} else if(arg > 20){
					phi = emissivity / absorption * Math.pow(inputData.getSourceSize(), 2);
				} else {
					phi =  Math.pow(inputData.getSourceSize(),2) * emissivity / absorption * (1-Math.exp(-arg));
				}
				return phi;
			}
			
			public Double getEmissivityPolarization() {
				return emissivityPolarization;
			}

			public Double getFluxPolarization() {
				return fluxPolarization;
			}

			public Double getPhiOrd() {
				return phiOrd;
			}

			public Double getPhiExtraord() {
				return phiExtraord;
			}

			public Double getPhiTotal() {
				return phiTotal;
			}

			public Double getQ() {
				return Q;
			}

			public Double getV() {
				return V;
			}
			
			
		}
		
	}
}
