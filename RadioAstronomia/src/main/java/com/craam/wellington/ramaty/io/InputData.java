package com.craam.wellington.ramaty.io;

import com.craam.wellington.interfaces.IInput;
import com.craam.wellington.interfaces.IOutput;
import com.craam.wellington.statics.Constants;
import com.craam.wellington.statics.Messages;

/*
 *  Essa classe é o objeto que carrega todas as informações
 *  de input de configuração para execução do algoritmo.
 *  
*/

public final class InputData {

	private Double energyEspectrum; // Espectro de energia;
	private Double electronNumber; // Número de electrons
	private Double magneticField; // Intensidade do campo magnético
	private int viewAngle; // Angulo formado entre o campo magnético e a linha de visada (em graus)
	private Double viewAngleCs, viewAngleSin; // Cosseno e Seno de viewAngle;
	private Double sourceSize; // Tamanho angular da fonte de emissão (rads) // Será convertido para arcsec
	private int energyLower; // Menor nível de energia usado na integração
	private int energyUpper; // Maior nível de energia usado na integração
	private int highestFrequency; // Valor da maior frequência
	private Double energyTreshould; // Valor de transição de energia
	private Double alpha; // Parâmentro de Razin (Girofrequência / Frequência de Plasma)
	
	public Double getEnergyEspectrum() {
		return energyEspectrum;
	}
	
	public Double getElectronNumber() {
		return electronNumber;
	}
	
	public Double getMagneticField() {
		return magneticField;
	}
	
	public Double getViewAngleCs() {
		return viewAngleCs;
	}

	public Double getViewAngleSin() {
		return viewAngleSin;
	}

	public int getViewAngle() {
		return viewAngle;
	}

	public Double getSourceSize() {
		return sourceSize;
	}
	
	public int getEnergyLower() {
		return energyLower;
	}
	
	public int getEnergyUpper() {
		return energyUpper;
	}
	
	public int getHighestFrequency() {
		return highestFrequency;
	}
	
	public Double getEnergyTreshould() {
		return energyTreshould;
	}
	
	public Double getAlpha() {
		return alpha;
	}
	
	public InputData setEnergyEspectrum(Double energyEspectrum) {
		this.energyEspectrum = energyEspectrum;
		return this;
	}
	
	public InputData setElectronNumber(Double electronNumber) {
		this.electronNumber = electronNumber;
		return this;
	}
	
	public InputData setMagneticField(Double magneticField) {
		this.magneticField = magneticField;
		return this;
	}
	
	public InputData setViewAngle(int viewAngle) throws Exception {
		if(viewAngle > 0 && viewAngle < 90){
			this.viewAngle = viewAngle;
			this.viewAngleCs = Math.cos(Math.toRadians(this.viewAngle));
			this.viewAngleSin = Math.sin(Math.toRadians(this.viewAngle));
			
		} else throw new Exception(Messages.VIEWANGLE_OUT_RANGE);
		return this;
	}
	
	public InputData setSourceSize(Double sourceSize) {
		this.sourceSize = sourceSize / Constants.ARCSEC_TO_RAD;
		return this;
	}
	
	public InputData setEnergyLower(int energyLower) {
		this.energyLower = energyLower;
		return this;
	}
	
	public InputData setEnergyUpper(int energyUpper) {
		this.energyUpper = energyUpper;
		return this;
	}
	
	public InputData setHighestFrequency(int highestFrequency) {
		this.highestFrequency = highestFrequency;
		return this;
	}
	
	public InputData setEnergyTreshould(Double energyTreshould) {
		this.energyTreshould = energyTreshould;
		return this;
	}
	
	public InputData setAlpha(Double alpha) {
		this.alpha = alpha;
		return this;
	}
	
}
