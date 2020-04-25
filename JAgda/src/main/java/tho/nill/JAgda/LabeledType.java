package tho.nill.JAgda;

public class LabeledType extends AgdaType {
	private int level;
	
	public LabeledType(int level) {
		super("Set",true,AgdaModul.NN,null);
		this.level = level;
	}

	@Override
	public boolean isSubtype(AgdaType type) {
		if (type instanceof LabeledType) {
			LabeledType other = (LabeledType)type;
			return other.level <= this.level;
		}
		return true;
	}
	
	@Override
	public String toString() {
		return " " + getSimpleName() + level + " ";
	}

	public AgdaType getType() {
		return new LabeledType(level+1);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + level;
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
		LabeledType other = (LabeledType) obj;
		if (level != other.level)
			return false;
		return true;
	}

}
