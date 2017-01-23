package com.craam.wellington.synclab;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.craam.wellington.interfaces.IAlgorithm;
import com.craam.wellington.interfaces.IInput;
import com.craam.wellington.interfaces.IOutput;
import com.craam.wellington.synclab.io.*;
import com.craam.wellington.statics.Constants;
import com.craam.wellington.utils.*;

public class SyncLab implements IAlgorithm {
	
	// Objetos de I/O
	InputData inputData;
	IOutput output;
	
	// Valores físicos 
	private Double gamma; // Gamma de Lorentz
	private Double electronFractionISR; // Fração de Elétrons particiando ISR;
	private Double electronFractionCSR; // Fração de Elétrons participando de CSR
	private Double electronNumberISR; //  Número de Elétrons participando de ISR
	private Double electronNumberCSR; //  Número de Elétrons participando de CSR
	
	private Double scaleFactorIe;
	private Double scaleFactorPe;
	private Double criticalFrequency; 

	// Vetores de valores físicos
	List<Frequency> frequencies = new ArrayList<Frequency>();

	public SyncLab(IInput input, IOutput output) {
		
		long start = System.nanoTime();
		
		// Carregando objetos de I/O
		this.inputData = (InputData) input.getInputData();
		this.output = output;

		this.gamma = (this.inputData.getElectronKinectyEnergy() / (Constants.ELECTRON_REST_ENERGY * Constants.MEV_TO_EV)) + 1;
		
		this.electronFractionISR = this.inputData.getElectronFractionISR();
		this.electronFractionCSR = (1 - this.electronFractionISR);
		this.electronNumberCSR = this.electronFractionCSR * this.inputData.getElectronNumber();
		this.electronNumberISR = this.electronFractionISR * this.inputData.getElectronNumber();
		
		this.scaleFactorIe = 3 * Math.pow(Constants.ELECTRON_CHARGE, 2) / (4 * Math.pow(Math.PI,2) * (Constants.LIGHT_SPEED * Constants.MS_TO_CMS)) * Constants.ERG_TO_JOULE;
		this.scaleFactorPe = Math.sqrt(3) * Math.pow(Constants.ELECTRON_CHARGE, 3) * this.inputData.getMagneticField() / (2* Math.PI * (Constants.ELECTRON_MASS * Constants.KG_TO_G) * (Math.pow(Constants.LIGHT_SPEED * Constants.MS_TO_CMS, 2))) * Constants.ERG_TO_JOULE;
		this.criticalFrequency = 3 * Math.pow(this.gamma,2) * Constants.ELECTRON_CHARGE * this.inputData.getMagneticField() / (4 * Math.PI * (Constants.ELECTRON_MASS * Constants.KG_TO_G) * (Constants.LIGHT_SPEED * Constants.MS_TO_CMS));

		// Inicia o cálculo entre os índices de frequência
		for(int i=1; i<=this.inputData.getHighestFrequency(); i++){
			frequencies.add(new Frequency(i));
		}
		output.generateOutput(this);
		long finish = System.nanoTime();
		System.out.println((finish-start)/1000000000.0);
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
		private Double x;
		private Double Ie, Pe;
		private Double d2WdvdO,dPdv;
		
		protected Frequency(int frequencyIndex){
			this.frequencyIndex = frequencyIndex;
			this.frequencyValue = Math.pow(10, (1e-2*(this.frequencyIndex-1))) * 1e9;
			this.x = frequencyValue / criticalFrequency;
			
			this.Ie = (2 * Math.PI) * scaleFactorIe * Math.pow(gamma,2) * H2(this.x);
			this.d2WdvdO = this.MultiParticleEFactor() * this.Ie;
			
			this.Pe = (2 * Math.PI) * scaleFactorPe * this.G2(this.x);
			this.dPdv = this.MultiParticleEFactor() * this.Pe;
		}
		
		protected Double FormFactor(){
			// Cosseno Hiperbólico
			//return 1 / (Math.cosh(Math.PI * inputData.getMicrobunchingTimeWidth() * this.frequencyValue / 2));
			// Gaussiana
			return Math.exp(-Math.pow((2 * inputData.getMicrobunchingTimeWidth() * this.frequencyValue), 2));
		}

		protected Double MultiParticleEFactor(){
			return electronNumberISR + electronNumberCSR * (1 - this.FormFactor()) + Math.pow(electronNumberCSR, 2) * this.FormFactor();
		}

		protected Double H2(Double x){ // H2 = X^2*K(2/3,X/2)^2
			Double fx1;
			
			if(x <= (1e-2))	fx1 = 4 * Math.PI / Math.sqrt(3) / 2.67894 * Math.pow((x/4),0.333) * (1 - 2.67894 / 0.902745 * Math.pow((x/4),1.3333));
			else
			if(x > 50) fx1 = Math.sqrt(Math.PI * x/4) * Math.exp(-x/2) * (1 + 7 / 72 /x * 2);
			else {
				fx1 = x * MathUtils.BesselKFrac(2./3., x/2);
			}
			
			return Math.pow(fx1, 2);
			
		}
		
		protected Double G2(Double x){ // G2 = X*Int_X^inf K(5/3,Y) dY
			Double fx2;
			
			if(x <= (1e-2)) fx2 = 4 * Math.PI / Math.sqrt(3) / 2.67894 * Math.pow((x/2),0.333) * (1 -2.67894 / 2 * Math.pow(x/2, 0.66666));
			else
			if(x > 10) fx2 = Math.sqrt(Math.PI*x/2) * Math.exp(-x) * (1 + 55. / 72 / x);
			
			else {
				
				Gauss gauss = new Gauss(inputData.getNY(), x, 200.);
				Double Integ = 0.;
				
				gauss.ScaleToAInf();
				
				Double  y[] = gauss.getGrid();
				Double wy[] = gauss.getWeights();
				
				for(int j=1; j<=inputData.getNY(); j++){
						Integ = Integ + (MathUtils.BesselKFrac(5./3., y[j]) * wy[j]);

				}
				fx2 = x * Integ;
			}
			
		return fx2;
				
		}
		
		public int getFrequencyIndex(){
			return this.frequencyIndex;
		}
	
		public Double getFrequency(){
			return this.frequencyValue;
		}

		public Double getIe() {
			return Ie;
		}

		public Double getPe() {
			return Pe;
		}

		public Double getD2WdvdO() {
			return d2WdvdO;
		}

		public Double getdPdv() {
			return dPdv;
		}
			
	}
}
