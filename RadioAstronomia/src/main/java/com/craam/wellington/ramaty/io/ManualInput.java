package com.craam.wellington.ramaty.io;

import java.util.HashMap;

import com.craam.wellington.interfaces.IInput;

/* Essa classe é utilizada para captar os dados de parâmetros de entrada
 * E carregar no objeto InputData. Implementa a Interface de mesmo nome.
 * Essa arquitetura pode ser utilizada para reutilização do código captando
 * informações de diferentes fontes de dados, basta implementar a interface.
 * Neste caso mais simples, o objeto será carregado com um HashMap; 
 */

public class ManualInput implements IInput{
	
	private InputData inputData;
	
	public ManualInput(HashMap<String, Double> map){
		
		this.inputData = new InputData();
		this.inputData.setEnergyEspectrum(map.get("ex"))
					  .setElectronNumber(map.get("anor"))
					  .setMagneticField(map.get("bmag"))
					  .setSourceSize(map.get("theta"))
					  .setEnergyLower(map.get("j1").intValue())
					  .setEnergyUpper(map.get("j2").intValue())
					  .setHighestFrequency(map.get("kf").intValue())
					  .setEnergyTreshould(map.get("etr"))
					  .setAlpha(map.get("alpha"));

		try{ this.inputData.setViewAngle(map.get("angle").intValue()); }
		catch(Exception e){}
	
	}
	
	public InputData getInputData(){
		return this.inputData;
	}

}
