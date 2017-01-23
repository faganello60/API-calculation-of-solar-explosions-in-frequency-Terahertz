package com.craam.wellington.synclab.io;

import java.util.HashMap;

import com.craam.wellington.interfaces.IInput;
import com.craam.wellington.statics.Constants;

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
		this.inputData.setMagneticField(map.get("bmag"))
					  .setElectronNumber(map.get("ne"))
					  .setMicrobunchingTimeWidth(map.get("tp"))
					  .setElectronKinectyEnergy(map.get("ek") * Constants.MEV_TO_EV)
					  .setHighestFrequency(map.get("kf").intValue())
					  .setNY(map.get("ny").intValue())
					  .setElectronFractionISR(map.get("xnisr"));
	}
	
	public InputData getInputData(){
		return this.inputData;
	}

}
