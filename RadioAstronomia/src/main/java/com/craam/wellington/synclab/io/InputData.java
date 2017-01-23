package com.craam.wellington.synclab.io;

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

	private int NY; // DEFINIR (parametro da função de Gauss)
	private int highestFrequency; // Valor da maior frequência
	private Double magneticField; // Intensidade do campo magnético
	private Double electronNumber; // Número de electrons
	private Double microbunchingTimeWidth; // Tempo de microbunching
	private Double electronKinectyEnergy; // Energia cinética do elétron
	private Double electronFractionISR; // Fração de elétrons participando de ISC
	
	public int getNY() {
		return NY;
	}
	public int getHighestFrequency() {
		return highestFrequency;
	}
	public Double getMagneticField() {
		return magneticField;
	}
	public Double getElectronNumber() {
		return electronNumber;
	}
	public Double getMicrobunchingTimeWidth() {
		return microbunchingTimeWidth;
	}
	public Double getElectronKinectyEnergy() {
		return electronKinectyEnergy;
	}
	public Double getElectronFractionISR() {
		return electronFractionISR;
	}
	public InputData setNY(int nY) {
		NY = nY;
		return this;
	}
	public InputData setHighestFrequency(int highestFrequency) {
		this.highestFrequency = highestFrequency;
		return this;
	}
	public InputData setMagneticField(Double magneticField) {
		this.magneticField = magneticField;
		return this;
	}
	public InputData setElectronNumber(Double electronNumber) {
		this.electronNumber = electronNumber;
		return this;
	}
	public InputData setMicrobunchingTimeWidth(Double microbunchingTimeWidth) {
		this.microbunchingTimeWidth = microbunchingTimeWidth;
		return this;
	}
	public InputData setElectronKinectyEnergy(Double electronKinectyEnergy) {
		this.electronKinectyEnergy = electronKinectyEnergy;
		return this;
	}
	public InputData setElectronFractionISR(Double electronFractionISR) {
		this.electronFractionISR = electronFractionISR;
		return this;
	}
}
