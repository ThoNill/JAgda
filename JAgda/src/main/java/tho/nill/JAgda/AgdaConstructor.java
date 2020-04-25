package tho.nill.JAgda;

public abstract class AgdaConstructor extends CalculationSteps implements CalculationStep {

	private AgdaType type;
	private AgdaType returnType;
	
	
	public AgdaConstructor(String simpleName, boolean visible,WithConstructorsType type,AgdaType returnType) {
		super(simpleName,visible, type.getModul());
		this.type = type;
		this.returnType = returnType;
		type.add(this);
	}


	public AgdaType getType() {
		return type;
	}


	public AgdaType getReturnType() {
		return returnType;
	}


	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((returnType == null) ? 0 : returnType.hashCode());
		result = prime * result + ((type == null) ? 0 : type.hashCode());
		return result;
	}
	
	
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		AgdaConstructor other = (AgdaConstructor) obj;
		if (returnType == null) {
			if (other.returnType != null)
				return false;
		} else if (!returnType.equals(other.returnType))
			return false;
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		return true;
	}


}

