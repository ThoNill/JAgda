package tho.nill.JAgda;

import java.util.List;

public class AgdaModul extends AgdaObject {
	public static final AgdaModul NN = new AgdaModul("",true,null);
	
	private List<AgdaType> types;
	private List<AgdaModul> submodules;
	private List<AgdaName> names;
	private List<FunctionType> functions;

	public AgdaModul(String simpleName, boolean visible, AgdaModul modul) {
		super(simpleName, visible, modul);
	}



}
