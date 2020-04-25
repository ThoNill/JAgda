package tho.nill.JAgda;

public class ConstructorInstance extends AgdaInstance {
	private AgdaConstructor constructor;

	public ConstructorInstance(AgdaType type, Object instance,AgdaConstructor constructor) {
		super(type, instance);
		this.constructor = constructor;
	}

	public AgdaConstructor getConstructor() {
		return constructor;
	}

	@Override
	public String toString() {
		Object o = getInstance();
		return "( " + constructor + ((o==null) ? "" : o.toString()) + ")";
	}



}
