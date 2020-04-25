package tho.nill.JAgda;

public abstract class AgdaFunction extends CalculationSteps {
	private FunctionType type;
	
	public AgdaFunction(String simpleName, boolean visible, AgdaModul modul,FunctionType type) {
		super(simpleName, visible, modul);
		this.type = type;
	}

	public FunctionType getType() {
		return type;
	}
	
}
