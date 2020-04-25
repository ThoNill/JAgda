package tho.nill.JAgda;

public abstract class AgdaType extends AgdaObject {
	public static AgdaType SET = new LabeledType(0);
	
	private AgdaType type;

	public AgdaType(String simpleName, boolean visible, AgdaModul modul,AgdaType type) {
		super(simpleName, visible, modul);
		this.type = type;
	}

	public abstract boolean isSubtype(AgdaType type);
	
	
	public boolean leq(AgdaType type) {
		return type.isSubtype(this);
	}
	
	public boolean isInstance(AgdaInstance  instance) {
		return isSubtype(instance.getType());
	}


	public AgdaType getType() {
		return type;
	}
	
}
