package tho.nill.JAgda;

public class AgdaMonad {
	private AgdaModul module;

	public AgdaMonad(AgdaModul module) {
		super();
		this.module = module;
	}

	public AgdaModul getModule() {
		return module;
	}

	public AgdaInstance apply(AgdaInstance source,CalculationSteps steps) {
		AgdaInstance value = source;
		for (CalculationStep step : steps.getSteps()) {
			value = step.apply(value); 
		}
		return value;
	}

}
